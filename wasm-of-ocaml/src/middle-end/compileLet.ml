open LinastUtils
open Linast
open Typedtree
open Types
open Asttypes

let rec expand_ors pat = match pat.pat_desc with
  | Tpat_or(p1, p2, _) -> (expand_ors p1) @ (expand_ors p2)
  | _ -> [pat]

(* Aliases case still needed here as it is used by simplified_or_patterns i.e. before removal of aliases *)
let rec is_variable_pattern pat = match pat.pat_desc with
  | Tpat_any | Tpat_var(_, _) -> true
   | Tpat_alias(p, _, _) -> is_variable_pattern p
  | _ -> false

let is_constructor_pattern pat = match pat.pat_desc with
  | Tpat_constant _ | Tpat_tuple _ | Tpat_construct(_, _, _) | Tpat_record(_, _) | Tpat_array _ -> true
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

let rec compile_matrix ~exported fail values matrix =
  let matrix = List.map (preprocess_row ~exported values) matrix in
  match (values, matrix) with
  | (_, []) -> LinastExpr.compound (Compound.fail fail) (* No valid patterns left *)
  | ([], ([], act)::rest) -> act
  (* OR rule -> Compile([v], expanded_or_matrix); Compile(vs, rest_of_row) *)
  | (v::vs, [(({pat_desc=Tpat_or(_, _, _)} as p)::ps, act)]) ->
    let patterns = expand_ors p in
    let (or_match, or_setup) = CompileMatch.compile_matrix fail [v] (* Need a compound for LinastExpr.seq *)
      (List.map (fun p -> ([p], ((Compound.imm unit_value, []), []), None)) patterns) in
    let rest = compile_matrix ~exported fail vs [(ps, act)] in
    binds_to_anf ~exported or_setup (LinastExpr.seq or_match rest)

  (* variable rule *)
  | (v::vs, matrix) when List.for_all
      (function ([], _) -> failwith "Not Possible to have empty list"
              | (p::ps,_) -> is_variable_pattern p) matrix ->
    compile_matrix ~exported fail vs (List.map (apply_variable_rule ~exported v) matrix)

  (* mixture rule *)
  | (v::vs, matrix) when List.exists
      (function ([], _) -> failwith "Not Possible to have empty list"
              | (p::ps,_) -> not(is_constructor_pattern p)) matrix ->
    let m1, m2 = apply_mixture_rule matrix in
    let new_fail = CompileMatch.next_fail_count () in
    let tree1 = compile_matrix ~exported new_fail values m1 in
    let tree2 = compile_matrix ~exported fail values m2 in
    LinastExpr.compound (Compound.matchtry new_fail tree1 tree2)

  | (v::vs, ({pat_desc=Tpat_tuple l}::_,_)::_) ->
      apply_tuple_rule ~exported fail v vs matrix (List.length l)
  | (v::vs, ({pat_desc=Tpat_record (_, _)}::_,_)::_) ->
        apply_record_rule ~exported fail v vs matrix
  | (v::vs, ({pat_desc=Tpat_array _}::_,_)::_) ->
      apply_array_rule ~exported fail v vs matrix
  | (v::vs, ({pat_desc=Tpat_construct (_, signature, _)}::_,_)::_) when signature.cstr_nonconsts = 0 ->
    let get_cstr_tag = function
      | ({pat_desc=Tpat_construct (_, desc, _)}::_,_) -> get_const_constructor_tag desc.cstr_tag
      | _ -> failwith "Can't apply constructor rule" in
    let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
      if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in
    (* Replace with int tags *)
    let matrix' = List.map (function (({pat_desc=Tpat_construct (_, desc, _)} as p)::ps,act) ->
      ({p with pat_desc=Tpat_constant(Asttypes.Const_int (get_const_constructor_tag desc.cstr_tag))}::ps, act)
      | _ -> failwith "Not a constructor pattern") matrix in
    apply_const_int_rule ~exported fail v vs matrix' cstrs_used (List.length cstrs_used = signature.cstr_consts)

 | (v::vs, ({pat_desc=Tpat_construct (_, signature, _)}::_,_)::_) ->
   apply_constructor_rule ~exported fail v vs matrix (signature.cstr_consts + signature.cstr_nonconsts)

  | (v::vs, ({pat_desc=Tpat_constant (Const_int _)}::_,_)::_) ->
    let get_const_int = function
      | ({pat_desc=Tpat_constant (Const_int i)}::_,_) -> i
      | _ -> failwith "Can't apply const_int rule" in
    let ints_used = List.fold_left (fun ints row -> let n = get_const_int row in
      if List.mem n ints then ints else n::ints) [] matrix in
    apply_const_int_rule ~exported fail v vs matrix ints_used false
  | (v::vs, ({pat_desc=Tpat_constant (Const_float _)}::_,_)::_) ->
     apply_float_rule ~exported fail v vs matrix
  | (v::vs, ({pat_desc=Tpat_constant _}::_,_)::_) ->raise (NotImplemented __LOC__)
  | _ -> failwith "Malformed matrix/vector input"

and apply_tuple_rule ~exported fail v vs matrix len =
  let new_val_ids = List.init len (fun _ -> Ident.create_local "tuple_arg") in
  let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
  let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
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
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
    let tree = compile_matrix ~exported fail (new_vals @ vs) new_matrix in
    (n, binds_to_anf ~exported new_val_binds tree)
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
  (LinastExpr.compound (Compound.mkswitch len_imm cases (Some (LinastExpr.compound (Compound.fail fail)))))

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
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
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
    (Some (LinastExpr.compound (Compound.fail fail)))))

and apply_record_rule ~exported fail v vs matrix =
    let get_label_pos (_, desc, _) = desc.lbl_pos in
    let rec get_labels_used acc = function
      | ({pat_desc=Tpat_record (l, _)}::_,_) ->
        List.fold_left (fun acc lbl -> let pos = get_label_pos lbl in
        if List.mem pos acc then acc else pos::acc) acc l
      | _ -> failwith "Can't apply record rule" in
    let labels_used = List.fold_left get_labels_used [] matrix in
    let new_val_ids = List.map (fun _ -> Ident.create_local "record_field") labels_used in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.map (fun (i, id) -> BLet(id, Compound.field v i))
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

(* Total=true when created from full constant constructor signature *)
(* Working out ints_used factored out to allow computing if total or not for const constructors *)
and apply_const_int_rule ~exported fail v vs matrix ints_used total =
  let specialise_const_int_matrix n =
    let rows = List.filter (function
      | ({pat_desc=Tpat_constant (Const_int m)}::_,_) -> m = n
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | (_::ps,act) -> (ps, act)
     | _ -> failwith "Wrong rule applied") rows in
    let action = compile_matrix ~exported fail vs new_matrix in
    (n, action) in
  let cases = List.map specialise_const_int_matrix ints_used in
  if total then LinastExpr.compound (Compound.mkswitch v cases None) else
  LinastExpr.compound (Compound.mkswitch v cases (Some (LinastExpr.compound (Compound.fail fail))))

and apply_float_rule ~exported fail v vs matrix =
  let specialise_float_matrix f =
    let rows = List.filter (function (* All constants must be floats due to type-checking *)
      | ({pat_desc=Tpat_constant c}::_,_) -> f = c
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | (_::ps,act) -> (ps, act)
     | _ -> failwith "Wrong rule applied") rows in
    let action = compile_matrix ~exported fail vs new_matrix in
    (f, action) in
  let get_float = function
        | ({pat_desc=Tpat_constant c}::_,_) -> c
        | _ -> failwith "Can't apply float rule" in
  let floats_used = List.fold_left (fun floats row -> let f = get_float row in
    if List.mem f floats then floats else f::floats) [] matrix in
  let cases = List.map specialise_float_matrix floats_used in
  (* Made awkward by wanting to return a compound *)
  List.fold_right (fun (f, body) rest ->
    let test_result = Compound.binary Eq (Imm.const f) v in
    let testid = Ident.create_local "isequal" in
    binds_to_anf ~exported [BLet(testid, test_result);] (LinastExpr.compound (Compound.mkif (Imm.id testid) body rest)))
    cases (LinastExpr.compound (Compound.fail fail))