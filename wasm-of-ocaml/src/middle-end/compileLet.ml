open LinastUtils
open Linast
open Typedtree
open Types
open Asttypes

(* Identical to compileMatch but actions/results are Linast expressions, not compounds/setups
   Used for compiling top level bind lists (hence never any guards).
   Still uses the compileMatch version for things like OR patterns, where we want a compound to use
   in a Linast.seq
   TODO: Unify files where possible *)
let fail_trap = -1l

let fail_count = ref fail_trap

let next_fail_count () =
  fail_count := Int32.add 1l (!fail_count);
  !fail_count

let rec expand_ors pat = match pat.pat_desc with
  | Tpat_or(p1, p2, _) -> (expand_ors p1) @ (expand_ors p2)
  | _ -> [pat]

(* Aliases case still needed here as it is used by simplified_or_patterns i.e. before removal of aliases *)
(* TODO: Variant patterns? *)
let rec is_variable_pattern pat = match pat.pat_desc with
  | Tpat_any | Tpat_var(_, _) -> true
   | Tpat_alias(p, _, _) -> is_variable_pattern p
  | _ -> false

(* Loose definition of constructor pattern, should also handle tuples/arrays/constants/records *)
(* Will likely need to split up actual case of constructor/constant(/array by length?) vs tuple/record *)
let is_constructor_pattern pat = match pat.pat_desc with
  | Tpat_constant _ | Tpat_tuple _ | Tpat_construct(_, _, _) | Tpat_record(_, _) | Tpat_array _ -> true
(*  | Tpat_alias(p, _, _) -> is_constructor_pattern p -- Aliases preprocessed out before this happens *)
  | _ -> false

let rec apply_variable_rule ~exported (value : Linast.imm_expr) (pats, action) =
    match pats with [] -> failwith "Not Possible to have empty list"
      | p::ps ->
      (match p.pat_desc with
        | Tpat_any -> (ps, action)
        | Tpat_var(x, _) ->
        (* TODO: Hack to avoid binding ident to itself at top of a function, remove by an optimisation pass *)
        let new_bind = match value.desc with ImmIdent i when i = x -> [] | _ -> [BLet(x, Compound.imm value)] in
        (ps, binds_to_anf ~exported new_bind action)
        | _ -> failwith "Not possible to apply variable rule")

(* TODO: Handle more general constants *)
let rec specialise_const_int_matrix matrix n =
  let rows = List.filter (function
    | ({pat_desc=Tpat_constant (Const_int m)}::_,_) -> m = n
    | _ -> failwith "Wrong rule applied") matrix in
  let new_matrix = List.map (function
   | (_::ps,act) -> (ps, act)
   | _ -> failwith "Wrong rule applied") rows in
  (n, new_matrix)

let rec apply_mixture_rule matrix =
  let rec split_by_test test acc = function
    | [] -> acc, []
    | (((p::_, _) as row)::rest) as matrix -> if test p then
      split_by_test test (row::acc) rest else acc, matrix
    | _ -> failwith "Mixture rule given empty pattern, should have been picked up as success already"
      in
  match matrix with
    | ((({pat_desc=Tpat_or(_, _, _)}::_, _) as row)::rest) -> [row], rest
    | (((p::_, _) as row)::rest) -> if is_variable_pattern p then split_by_test is_variable_pattern [row] rest
      else if is_constructor_pattern p then split_by_test is_constructor_pattern [row] rest
      else failwith "Mixture rule ran into matrix it couldn't process"
    | _ -> failwith "Mixture rule ran into a matrix it couldn't process"

(* p1|...|x|pk|...|pn -> p1|...|x *)
let rec simplified_or_pattern pat = match pat.pat_desc with
  | Tpat_or(p1, p2, r) -> if is_variable_pattern p1 then Some p1
    else let simpl1 = simplified_or_pattern p1 and simpl2 = simplified_or_pattern p2 in
    (match (simpl1, simpl2) with
        | (None, None) -> None
        | (Some p, _) -> Some p
        | (None, Some p) -> Some {pat with pat_desc = Tpat_or(p1, p, r)}
    )
  | _ -> None

(* For the first column, rewrite any aliases to just happen in the action, and simplify OR patterns *)
let rec preprocess_row ~exported values ((patterns, act) as row) = match values, patterns with
  | [], [] -> row
  | v::vs, {pat_desc=Tpat_alias(p, id, _)}::ps ->
    preprocess_row values ~exported (p::ps, binds_to_anf ~exported [BLet(id, Compound.imm v)] act)
  | v::vs, p::ps -> (match simplified_or_pattern p with None -> row
    | Some p -> preprocess_row ~exported values (p::ps, act))
  | [], p::ps -> failwith "Malformed row, value forgotten"
  | _, _ -> failwith "malformed row"

(* Add partial information later *)
(* values is imm vector corresponding to the vector of patterns in each row *)
(* List of rows, each row is ([pattern list], (action, action_setup), (guard,setup) option)
   action/guard is a compound *)
(* TODO: May need an extra rule for variants and how to process them, shouldn't be difficult *)
(* TODO: Make use of guards/cleaner pattern matching to avoid checks that lists not empty *)
(* TODO: GUARDS PUT IN WHEN TRANSLATING MATCH STATEMENT TO INITIAL MATRIX *)
let rec compile_matrix ~exported fail values matrix =
  let matrix = List.map (preprocess_row ~exported values) matrix in
  match (values, matrix) with
  | (_, []) -> LinastExpr.compound (Compound.imm (Imm.fail fail)) (* No valid patterns left *)
  | ([], ([], act)::rest) -> act
  (* OR rule -> Compile([v], expanded_or_matrix); Compile(vs, rest_of_row) *)
  | (v::vs, [(({pat_desc=Tpat_or(_, _, _)} as p)::ps, act)]) ->
    let patterns = expand_ors p in
    let (or_match, or_setup) = CompileMatch.compile_matrix fail [v] (* Need a compound for LinastExpr.seq *)
      (List.map (fun p -> ([p], ((Compound.imm unit_value, []), []), None)) patterns) in
    let rest = compile_matrix ~exported fail vs [(ps, act)] in
    binds_to_anf ~exported or_setup (LinastExpr.seq or_match rest)

  | (v::vs, matrix) ->
    if List.for_all
      (function ([], _) -> failwith "Not Possible to have empty list" | (p::ps,_) -> is_variable_pattern p) matrix
    then (* variable rule *)
       compile_matrix ~exported fail vs (List.map (apply_variable_rule ~exported v) matrix)
    else if List.for_all
   (function ([], _)  -> failwith "Not Possible to have empty list" | (p::ps,_) -> is_constructor_pattern p) matrix
    then (* constructor rule TODO: Move each case to a mutually recursive function *)
      (match matrix with
        | ({pat_desc=Tpat_tuple l}::_,_)::_ ->
         apply_tuple_rule ~exported fail v vs matrix (List.length l)

         (* Handle constant constructors specially *)
         | ({pat_desc=Tpat_construct (_, signature, _)}::_,_)::_ when signature.cstr_nonconsts = 0 ->
           let get_cstr_tag = function
              | ({pat_desc=Tpat_construct (_, desc, _)}::_,_) -> (* TODO: Change what get_const_constructor_tag returns? *)
                (match get_const_constructor_tag desc.cstr_tag with Const_int i -> i | _ -> failwith "const_constructor_tag")
              | _ -> failwith "Can't apply constructor rule" in
           let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
             if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in
          (* Replace with int tags *)
          let matrix' = List.map (function (({pat_desc=Tpat_construct (_, desc, _)} as p)::ps,act) ->
            ({p with pat_desc=Tpat_constant(get_const_constructor_tag desc.cstr_tag)}::ps, act)
            | _ -> failwith "Not a constructor pattern") matrix in
          (* Now just treat as int constant since each pattern is const_int *)
        let cases = List.map (specialise_const_int_matrix matrix') cstrs_used in
        let cases' = List.map (fun (n, mat) -> let action = compile_matrix ~exported fail vs mat
          in (Int32.of_int n, action)) cases in
          if List.length cstrs_used = signature.cstr_consts then (* Reason for not doing as pre-processing, can tell if all int cases considered *)
          LinastExpr.compound (Compound.mkswitch v cases' None) else
        LinastExpr.compound (Compound.mkswitch v cases' (Some (LinastExpr.compound (Compound.imm (Imm.fail fail)))))

        | ({pat_desc=Tpat_construct (_, signature, _)}::_,_)::_ ->
          (* TODO: When is cstr_normal non-zero? *)
          apply_constructor_rule ~exported fail v vs matrix (signature.cstr_consts + signature.cstr_nonconsts)

        | ({pat_desc=Tpat_record (_, _)}::_,_)::_ ->
          apply_record_rule ~exported fail v vs matrix

        | ({pat_desc=Tpat_array _}::_,_)::_ ->
          apply_array_rule ~exported fail v vs matrix

        (* Similar to constructor case but just test each constant + have a default fail
           TODO: Sometimes better to use if/else rather than a switch? e.g. only 1 row? *)
        | ({pat_desc=Tpat_constant (Const_int _)}::_,_)::_ ->
          let get_const_int = function
            | ({pat_desc=Tpat_constant (Const_int i)}::_,_) -> i
            | _ -> failwith "Can't apply const_int rule" in
          let ints_used = List.fold_left (fun ints row -> let n = get_const_int row in
            if List.mem n ints then ints else n::ints) [] matrix in
          let cases = List.map (specialise_const_int_matrix matrix) ints_used in
          (* TODO: Just make specialise_const_int_matrix mutually recursive *)
          let cases' = List.map (fun (n, mat) -> let action = compile_matrix ~exported fail vs mat
            (* TODO: Need to be careful with mapping at wasm level of what does/doesn't get doubled *)
            in (Int32.of_int n, action)) cases in
          LinastExpr.compound (Compound.mkswitch v cases' (Some (LinastExpr.compound (Compound.imm (Imm.fail fail)))))

        (* TODO: Support chars/floats/strings/int32/64/native?
                 Linast has all of these but CSwitch only supports int32 tags *)
        | ({pat_desc=Tpat_constant _}::_,_)::_ ->raise (NotImplemented __LOC__)
        | _ -> failwith "Should never happen, wrong rule applied"
      )
    else (* mixture rule *)
      let m1, m2 = apply_mixture_rule matrix in
      let new_fail = next_fail_count () in
      let tree1 = compile_matrix ~exported new_fail values m1 in
      let tree2 = compile_matrix ~exported fail values m2 in
      LinastExpr.compound (Compound.matchtry
         new_fail
         tree1
         tree2)
  | _ -> failwith "Malformed matrix/vector input"

and apply_tuple_rule ~exported fail v vs matrix len =
  let new_val_ids = List.init len (fun _ -> Ident.create_local "tuple_arg") in
  let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
  let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v (Int32.of_int i))) new_val_ids in
  let new_rows = List.map
  (function ({pat_desc=Tpat_tuple l}::ps,act) -> (l@ps, act) | _ -> failwith "Wrong rule applied") matrix in
  let tree = compile_matrix ~exported fail (new_vals @ vs) new_rows in
  binds_to_anf ~exported new_val_binds tree

and apply_array_rule ~exported fail v vs matrix =
  let rec specialise_matrix n =
    let rows = List.filter (function
      | ({pat_desc=Tpat_array pl}::_,_) -> List.length pl = n
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | ({pat_desc=Tpat_array pl}::ps,act) -> (pl @ ps, act)
     | _ -> failwith "Wrong rule applied") rows in
    let new_val_ids = List.init n (fun _ -> Ident.create_local "array_arg") in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v (Int32.of_int i))) new_val_ids in
    let tree = compile_matrix ~exported fail (new_vals @ vs) new_matrix in
    (Int32.of_int n, binds_to_anf ~exported new_val_binds tree)
    in
  let get_length = function
    | ({pat_desc=Tpat_array l}::_,_) -> List.length l
    | _ -> failwith "Can't apply constructor rule" in
  let lengths_used = List.fold_left (fun lens row -> let n = get_length row in
    if List.mem n lens then lens else n::lens) [] matrix in
  let cases = List.map specialise_matrix lengths_used in
  let len_id = Ident.create_local "array_len" in
  let len_imm = Imm.id len_id in
  let len_bind = BLet(len_id, Compound.gettag v) in
  binds_to_anf ~exported [len_bind]
  (LinastExpr.compound (Compound.mkswitch len_imm cases (Some (LinastExpr.compound (Compound.imm (Imm.fail fail))))))

and apply_constructor_rule ~exported fail v vs matrix num_constructors =
  let rec specialise_constructor_matrix tag =
    let rows = List.filter (function (* Could just check equality of tag_desc's? *)
      | ({pat_desc=Tpat_construct (_, tag_desc, _)}::_, _) -> (unify_constructor_tag tag_desc) = tag
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | ({pat_desc=Tpat_construct (_, _, pl)}::ps,act) -> (pl @ ps, act)
     | _ -> failwith "Wrong rule applied") rows in
    let arity = match rows with ({pat_desc=Tpat_construct (_, _, pl)}::_,_)::_ -> List.length pl
      | _ -> failwith "Wrong rule applied" in
    let new_val_ids = List.init arity (fun _ -> Ident.create_local "cstr_arg") in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v (Int32.of_int i))) new_val_ids in
    let tree = compile_matrix ~exported fail (new_vals @ vs) new_matrix in
    (tag, binds_to_anf ~exported new_val_binds tree) in

  let get_cstr_tag = function
    | ({pat_desc=Tpat_construct (_, tag_desc, _)}::_,_) -> unify_constructor_tag tag_desc
    | _ -> failwith "Can't apply constructor rule" in
  let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
    if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in
  let cases = List.map specialise_constructor_matrix cstrs_used in
  let tag_id = Ident.create_local "cstr_tag" in
  let tag_imm = Imm.id tag_id in
  let tag_bind = BLet(tag_id, Compound.gettag v) in
  if List.length cstrs_used = num_constructors
  then binds_to_anf ~exported [tag_bind] (LinastExpr.compound (Compound.mkswitch tag_imm cases None))
  else binds_to_anf ~exported [tag_bind] (LinastExpr.compound (Compound.mkswitch tag_imm cases
    (Some (LinastExpr.compound (Compound.imm (Imm.fail fail))))))

and apply_record_rule ~exported fail v vs matrix =
    let get_label_pos (_, desc, _) = desc.lbl_pos in
    let rec get_labels_used acc = function
      | ({pat_desc=Tpat_record (l, _)}::_,_) ->
        (* TODO: Would be nice to put these in sorted order at end *)
        List.fold_left (fun acc lbl -> let pos = get_label_pos lbl in
        if List.mem pos acc then acc else pos::acc) acc l
      | _ -> failwith "Can't apply record rule" in
    let labels_used = List.fold_left get_labels_used [] matrix in
    let new_val_ids = List.map (fun _ -> Ident.create_local "record_field") labels_used in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.map (fun (i, id) -> BLet(id, Compound.field v (Int32.of_int i)))
     (List.combine labels_used new_val_ids) in

    let make_wildcard_pattern = function
      | (_, _, p)::_ -> {p with pat_desc = Tpat_any}
      | _ -> failwith "Typedtree says not possible, record patterns always has >0 sub-patterns" in
    (* map each pattern list to patterns | Tpat_any for each label in labels used *)
    let get_pattern wildcard lbl_list lbl_pos =
      match List.find_opt (fun lbl -> get_label_pos lbl = lbl_pos) lbl_list with
        | Some (_, _, p) -> p
        | None -> wildcard in

    let new_rows = List.map (* Each Tpat_record is replaced with a pattern for each label examined *)
      (function ({pat_desc=Tpat_record (l, _)}::ps,act) ->
         ((List.map (get_pattern (make_wildcard_pattern l) l) labels_used)@ps, act)
        | _ -> failwith "Wrong rule applied") matrix in
    let tree = compile_matrix ~exported fail (new_vals @ vs) new_rows in
    binds_to_anf ~exported new_val_binds tree (* Extract each of the fields then recursively match the pattern *)