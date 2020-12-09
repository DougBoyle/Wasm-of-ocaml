open LinastUtils
open Linast
open Typedtree
open Types

(* Used whenever a match could fail. Default value is -1 for trap, otherwise this is used to access an outer try/catch *)
(* Great simplification of OCaml's approach, which keeps a list of (int * matrix) pairs, allowing the compiler to not
   only escape a failed match, but potentially jump further down the list of cases based on additional information learned
   about the expression during trying this pattern. *)
let fail_trap = -1l

let fail_count = ref fail_trap

let next_fail_count () =
  fail_count := Int32.add 1l (!fail_count);
  !fail_count

(* TODO: Likely want to completely replace this to be based off of matrices and a value list *)
(* pattern -> comp_expr -> BLet list *)
(* value of expression needed in case that it first needs binding to something e.g. TPat_var or alias *)
(* For 'let pattern = expr ...' statements *)
(* Inefficient in some places when expr = Compound.Imm (Imm.id x) - creates another identifier bound to the same thing.
   Should be able to remove this when doing common subexpression elimination/dead assignment elimination. *)
let rec getBindings fail (pat : pattern) (expr : compound_expr) = match pat.pat_desc with
  (* Even if nothing gets bound, need to ensure expression is evaluated exactly once *)
  | Tpat_any -> [BEffect expr] (* TODO: Don't both if expr is an ident or some other non-effect expr *)
  (* TODO: Bit of a hack to avoid trying to bind an identifier to itself e.g. when param of function = only pattern
           Could likely remove in a cleanup pass, as it won't be causing any issues, just looks odd *)
  | Tpat_var (x, _) ->
    let default = [BLet(x, expr)] in
    begin match expr.desc with CImm{desc=ImmIdent i} -> if i = x then [] else default | _ -> default end
  | Tpat_alias (p, x, _) -> (BLet(x, expr)) :: getBindings fail p expr
  | Tpat_constant c ->
    let id = Ident.create_local "constant" in
    let test_result = Compound.binary Eq (Imm.const c) (Imm.id id) in
    let testid = Ident.create_local "isequal" in
    [BLet(id, expr); BLet(testid, test_result);
    BEffect(Compound.mkif
      (Imm.id testid)
      (LinastExpr.compound (Compound.imm unit_value))   (* Test passed - do nothing TODO: Use a global unit value *)
      (LinastExpr.compound (Compound.imm (Imm.fail fail))))] (* Test failed - trap/next pattern *)
  | Tpat_tuple l ->
    let id = Ident.create_local "tuple" in
    let tuple_imm = Imm.id id in
    let tuple_element_binds index pat =
      let arg_id = Ident.create_local "tuple_arg" in
      let arg_expr = Compound.imm (Imm.id arg_id) in
      let sub_binds = getBindings fail pat arg_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(arg_id, Compound.field tuple_imm (Int32.of_int index)))::sub_binds)
    in let binds = List.concat (List.mapi tuple_element_binds l) in
    (match binds with [] -> [] | _ -> (BLet(id, expr)::binds))

  (* Case when constructor is just treated as an integer *)
  | Tpat_construct (_, desc, []) when desc.cstr_nonconsts = 0 ->
    getBindings fail {pat with pat_desc=Tpat_constant(get_const_constructor_tag desc.cstr_tag)} expr

  | Tpat_construct (_, cstr_desc, pl) ->
    let id = Ident.create_local "construct" in
    let block_imm = Imm.id id in

    (* Test tag - fail if not a match *)
    let tag_id = Ident.create_local "construct_tag" in
    let tag_expr = Compound.gettag block_imm in
    (* Note that this puts an int32 constant in the tree, instead of the usual const_int - other integers aren't converted *)
    let test_result = Compound.binary Eq (Imm.const (Const_int32 (unify_constructor_tag cstr_desc))) (Imm.id tag_id) in
    let testid = Ident.create_local "isequal" in

    (* If tag matches - bind subcomponents *)
    let block_element_binds index pat =
      let elem_id = Ident.create_local "element" in
      let elem_expr = Compound.imm (Imm.id elem_id) in
      let sub_binds = getBindings fail pat elem_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(elem_id, Compound.field block_imm (Int32.of_int index)))::sub_binds)
    in let sub_binds = List.concat (List.mapi block_element_binds pl) in

    [BLet(id, expr);
    BLet(tag_id, tag_expr);
    BLet(testid, test_result);
    BEffect(Compound.mkif
      (Imm.id testid)
      (LinastExpr.compound (Compound.imm unit_value))   (* Test passed - do nothing *)
      (LinastExpr.compound (Compound.imm (Imm.fail fail)))); (* Test failed - trap/next pattern *)
    ]  @ sub_binds

  | Tpat_record (l, _) ->
    let id = Ident.create_local "record" in
    let record_imm = Imm.id id in
    let field_binds index (_, labelDesc, pat) =
      let field_id = Ident.create_local "field" in
      let field_expr = Compound.imm (Imm.id field_id) in
      let sub_binds = getBindings fail pat field_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(field_id, Compound.field record_imm (Int32.of_int labelDesc.lbl_pos)))::sub_binds)
    in let binds = List.concat (List.mapi field_binds l) in
    (match binds with [] -> [] | _ -> (BLet(id, expr)::binds))

  (* Unsure about OR case - how does it interact with matching single or multiple components? *)
  (* Is putting expr into an ident necessary? Avoids evaluating a compound twice, but many wasted bindings if nested ORs *)
  | Tpat_or (p1, p2, _) ->
    let id = Ident.create_local "or" in
    let expr' = Compound.imm (Imm.id id) in
    let new_fail = next_fail_count () in
    let first_binds = getBindings new_fail p1 expr' in
    let second_binds = getBindings fail p2 expr' in
    (* TODO: Annoying that we have to put a dummy 'unit' at the end of each case. May need an extra term to avoid this *)
    let compound = Compound.matchtry new_fail
      (binds_to_anf first_binds (LinastExpr.compound (Compound.imm unit_value))) (* Have to put dummy unit at end of tree *)
      (binds_to_anf second_binds (LinastExpr.compound (Compound.imm unit_value)))
    in [BLet(id, expr); BEffect compound]

 (* Identical to constructor, but check length rather than a given tag *)
 | Tpat_array pl ->
      let expected_length = List.length pl in
      let id = Ident.create_local "array" in
      let ar_imm = Imm.id id in

      (* Test tag - fail if not a match *)
      let len_id = Ident.create_local "array_len" in
      let len_expr = Compound.gettag ar_imm in
      (* Note that this puts an int32 constant in the tree, instead of the usual const_int - other integers aren't converted *)
      let test_result = Compound.binary Eq (Imm.const (Const_int32 (Int32.of_int expected_length))) (Imm.id len_id) in
      let testid = Ident.create_local "isequal" in

      (* If tag matches - bind subcomponents *)
      let block_element_binds index pat =
        let elem_id = Ident.create_local "element" in
        let elem_expr = Compound.imm (Imm.id elem_id) in
        let sub_binds = getBindings fail pat elem_expr in
        (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
        (match sub_binds with [] -> [] | _ -> (BLet(elem_id, Compound.field ar_imm (Int32.of_int index)))::sub_binds)
      in let sub_binds = List.concat (List.mapi block_element_binds pl) in

      [BLet(id, expr);
      BLet(len_id, len_expr);
      BLet(testid, test_result);
      BEffect(Compound.mkif
        (Imm.id testid)
        (LinastExpr.compound (Compound.imm unit_value))   (* Test passed - do nothing *)
        (LinastExpr.compound (Compound.imm (Imm.fail fail)))); (* Test failed - trap/next pattern *)
      ]  @ sub_binds


  | Tpat_variant (_, _, _) -> raise (NotImplemented __LOC__)
  | _ -> raise NotSupported

let include_guard fail expr = function
  | None -> (expr, [])
  | Some (comp, setup) ->
    let id = Ident.create_local "guard" in
    let id_imm = Imm.id id in
    (Compound.mkif (id_imm) (LinastExpr.compound comp) (LinastExpr.compound (Compound.imm (Imm.fail fail))),
     setup @ [BLet(id, comp)]) (* May as well just pass it in as an immediate? *)


(* First improvement should be to handle constructors at the start of several of the first patterns.
   Not entirely simple as this can still generate OR patterns (usually done as extra matrix rows) when 2
   patterns have the same constructor at their heads. Hence leave till matrix approach properly implemented *)

(* Just do most naive approach initially, optimise/do grouping once I have something that works at least.
   Can decide based on time available/how bad the initial output is. *)

(* Takes a (pattern * compound) list and Compound to match and outputs a compound (+ setup) which does the match + evaluation*)
(* Ocaml doesn't have ML's general `let f [] [] = a | (x::xs) _ = b | ...` syntax, only 1 expr/pattern to check naively *)
(* TODO: Check for exhaustiveness where possible *)
(* TODO: Use Switch whenever multiple constructor patterns found (partial -> result of the matching) *)
(* TODO: Split head off of each pattern rather than working down linearly one-at-a-time *)
(* Is there every actually any `setup` to do for pattern matching? All part of the binds return by getBinds *)
(* THIS SHOULD POSSIBLY RETURN A LINAST_EXPR INSTEAD OF A COMPOUND - MAKES MIDDLE CASE EASIER TOO *)
let rec compile_match partial fail expr = function
  | [] -> (Compound.imm (Imm.fail fail), []) (* All cases exhausted *)
  | [(pat, (e, setup), guard)] when (match partial with Total -> true | _ -> false) ->
    let binds = getBindings fail pat expr in
    let (e', guard_setup) = include_guard fail e guard in
    (e', binds @ guard_setup @ setup) (* Total so no matchtry, hence need to return the setup separately *)
  | (pat, (e, setup), guard)::rest -> let (rest, rest_setup) = compile_match partial fail expr rest in
    let new_fail = next_fail_count () in
    let binds = getBindings new_fail pat expr in
    let (e', guard_setup) = include_guard new_fail e guard in
    (Compound.matchtry
      new_fail
      (binds_to_anf (binds @ guard_setup @ setup) (LinastExpr.compound e'))
      (binds_to_anf rest_setup (LinastExpr.compound rest)),
    [])

(* --------------------------------------------------------------------- *)
(* TODO: Should also have pre-processing somewhere for bits like (_|p1) = _?
         Also handle guards by pre-processing *)

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

let rec apply_variable_rule value (pats, (action, action_setup)) =
    match pats with [] -> failwith "Not Possible to have empty list"
      | p::ps ->
      (match p.pat_desc with
        | Tpat_any -> (ps, (action, action_setup))
        | Tpat_var(x, _) -> (ps, (action, (BLet(x, Compound.imm value))::action_setup))
        | _ -> failwith "Not possible to apply variable rule")


let rec specialise_constructor_matrix v vs matrix tag =
  let rows = List.filter (function (* Could just check equality of tag_desc's? *)
    | ({pat_desc=Tpat_construct (_, tag_desc, _)}::ps,act) -> (unify_constructor_tag tag_desc) = tag
    | _ -> failwith "Wrong rule applied") matrix in
  let new_matrix = List.map (function
   | ({pat_desc=Tpat_construct (_, _, pl)}::ps,act) -> (pl @ ps, act)
   | _ -> failwith "Wrong rule applied") rows in
  let arity = match matrix with ({pat_desc=Tpat_construct (_, _, pl)}::_,_)::_ -> List.length pl | _ -> failwith "Wrong rule applied" in
  let new_val_ids = List.init arity (fun _ -> Ident.create_local "cstr_arg") in
  let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
  let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v (Int32.of_int i))) new_val_ids in
  (* tag, matrix, value vector, setup to prepend to recursive call *)
  (tag, new_matrix, new_vals @ vs, new_val_binds)

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
let rec preprocess_row values ((patterns, (action, action_setup)) as row) = match values, patterns with
  | [], [] -> row
  | v::vs, {pat_desc=Tpat_alias(p, id, _)}::ps ->
    preprocess_row values (p::ps, (action, (BLet(id, Compound.imm v))::action_setup))
  | v::vs, p::ps -> (match simplified_or_pattern p with None -> row
    | Some p -> preprocess_row  values (p::ps, (action, action_setup)))
  | _, _ -> failwith "malformed row"

(* Add partial information later *)
(* values is vector corresponding to the vector of patterns in each row *)
(* List of rows, each row is ([pattern list], (action, action_setup)) - action is a compound? *)
(* TODO: May need an extra rule for variants and how to process them, shouldn't be difficult *)
(* TODO: Make use of guards/cleaner pattern matching to avoid checks that lists not empty *)
(* TODO: GUARDS PUT IN WHEN TRANSLATING MATCH STATEMENT TO INITIAL MATRIX *)
let rec compile_matrix fail values matrix =
  let matrix = List.map (preprocess_row values) matrix in
  match (values, matrix) with
  | (_, []) -> (Compound.imm (Imm.fail fail), []) (* No valid patterns left *)
  | ([], ([], act)::rest) -> act
  (* OR rule -> Compile([v], expanded_or_matrix); Compile(vs, rest_of_row) *)
  | (v::vs, [(({pat_desc=Tpat_or(_, _, _)} as p)::ps, act)]) ->
    let patterns = expand_ors p in
    let (or_match, or_setup) = compile_matrix fail [v]
      (List.map (fun p -> ([p], (Compound.imm unit_value, []))) patterns) in
    let (rest, rest_setup) = compile_matrix fail vs [(ps, act)] in
    (rest, or_setup @ (BEffect(or_match))::rest_setup)

  | (v::vs, matrix) ->
    if List.for_all
      (function ([], _) -> failwith "Not Possible to have empty list" | (p::ps,_) -> is_variable_pattern p) matrix
    then (* variable rule *)
       compile_matrix fail vs (List.map (apply_variable_rule v) matrix)
    else if List.for_all
   (function ([], _)  -> failwith "Not Possible to have empty list" | (p::ps,_) -> is_constructor_pattern p) matrix
    then (* constructor rule TODO: Move each case to a mutually recursive function *)
      (match matrix with
        | ({pat_desc=Tpat_tuple l}::_,_)::_ ->
          let n = List.length l in
          let new_val_ids = List.init n (fun _ -> Ident.create_local "tuple_arg") in
          let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
          let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v (Int32.of_int i))) new_val_ids in
          let new_rows = List.map
          (function ({pat_desc=Tpat_tuple l}::ps,act) -> (l@ps, act) | _ -> failwith "Wrong rule applied") matrix in
          let (expr, setup) = compile_matrix fail (new_vals @ vs) new_rows in
          (expr, new_val_binds @ setup)

        | ({pat_desc=Tpat_construct (_, signature, _)}::_,_)::_ ->
          let get_cstr_tag = function
            | ({pat_desc=Tpat_construct (_, tag_desc, _)}::_,_) -> unify_constructor_tag tag_desc
            | _ -> failwith "Can't apply constructor rule" in
          let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
            if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in
          let cases = List.map (specialise_constructor_matrix v vs matrix) cstrs_used in
          (* TODO: Just make specialise_constructor_matrix mutually recursive *)
          let cases' = List.map (fun (tag, mat, vs, binds) -> let (expr, setup) = compile_matrix fail vs mat
            in (tag, binds_to_anf (binds @ setup) (LinastExpr.compound expr))) cases in
          let tag_id = Ident.create_local "cstr_tag" in
          let tag_imm = Imm.id tag_id in
          let tag_bind = BLet(tag_id, Compound.gettag v) in
          if List.length cstrs_used = signature.cstr_consts + signature.cstr_nonconsts (* TODO: cstr_normal?? *)
          then (Compound.mkswitch tag_imm cases' None, [tag_bind])
          else (Compound.mkswitch tag_imm cases' (Some (LinastExpr.compound (Compound.imm (Imm.fail fail)))), [tag_bind])
        (* Similar to tuple case but more work to determine which fields to extract/match against *)
        | ({pat_desc=Tpat_record (l, _)}::_,_)::_ -> (* See comment in Typedtree, always at least 1 pattern *)
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
            | _ -> failwith "Can't apply record rule to row with no record subpattern" in
          (* map each pattern list to patterns | Tpat_any for each label in labels used *)
          let get_pattern wildcard lbl_list lbl_pos =
            match List.find_opt (fun lbl -> get_label_pos lbl = lbl_pos) lbl_list with
              | Some (_, _, p) -> p
              | None -> wildcard in

          let new_rows = List.map (* Each Tpat_record is replaced with a pattern for each label examined *)
            (function ({pat_desc=Tpat_record (l, _)}::ps,act) ->
               ((List.map (get_pattern (make_wildcard_pattern l) l) labels_used)@ps, act)
              | _ -> failwith "Wrong rule applied") matrix in
          let (expr, setup) = compile_matrix fail (new_vals @ vs) new_rows in
          (expr, new_val_binds @ setup) (* Extract each of the fields then recursively match the pattern *)

        (* Similar to constructor case but tag is just length of pattern list *)
        | ({pat_desc=Tpat_array _}::_,_)::_ ->raise (NotImplemented __LOC__)
        (* Similar to constructor case but just test each constant + have a default fail
           TODO: Support floats/strings? *)
        | ({pat_desc=Tpat_constant _}::_,_)::_ ->raise (NotImplemented __LOC__)
        | _ -> failwith "Should never happen, wrong rule applied"
      )
    else (* mixture rule *)
      let m1, m2 = apply_mixture_rule matrix in
      let new_fail = next_fail_count () in
      let (expr1, setup1) = compile_matrix new_fail values m1 in
      let (expr2, setup2) = compile_matrix fail values m2 in
      (Compound.matchtry
         new_fail
         (binds_to_anf setup1 (LinastExpr.compound expr1))
         (binds_to_anf setup2 (LinastExpr.compound expr2)),
       [])
  | _ -> failwith "Malformed matrix/vector input"
