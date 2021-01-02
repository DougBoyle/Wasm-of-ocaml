open LinastUtils
open Linast
open Typedtree
open Types
open Asttypes

(* Default value is -1 for trap, otherwise this is used to access an outer try/catch *)
let fail_trap = -1l

let fail_count = ref fail_trap

let next_fail_count () =
  fail_count := Int32.add 1l (!fail_count);
  !fail_count

(* Use if-then-else rather than try/fail handlers to test each guard until an action succeeds *)
let rec include_guard fail = function
 (* out of cases, fail as above *)
 | [] -> (Compound.fail fail, [])
 (* No guard so guarenteed to succeed, can discard remaining rows *)
 | ([], ((action, action_setup), binds), None)::_ -> (action, binds @ action_setup)
 (* Test guard. If guard fails, recurse on remaining rows *)
 | ([], ((action, action_setup), binds), Some (guard_comp, guard_setup))::rest ->
    let rest_expr, rest_setup = include_guard fail rest in
    let id = Ident.create_local "guard" in
    let id_imm = Imm.id id in
    (Compound.mkif (id_imm) (binds_to_anf action_setup (LinastExpr.compound action))
     (binds_to_anf rest_setup (LinastExpr.compound rest_expr)),
     binds @ guard_setup @ [BLet(id, guard_comp)])
  | _ -> failwith "Value vector/pattern matrix mismatch. Pattern matrix expected to be empty"


(* Aliases case still needed here as it is used by simplified_or_patterns i.e. before removal of aliases *)
(* TODO: Variant patterns? *)
let rec is_variable_pattern pat = match pat.pat_desc with
  | Tpat_any | Tpat_var(_, _) -> true
   | Tpat_alias(p, _, _) -> is_variable_pattern p
  | _ -> false

let is_constructor_pattern pat = match pat.pat_desc with
  | Tpat_constant _ | Tpat_tuple _ | Tpat_construct(_, _, _) | Tpat_record(_, _) | Tpat_array _ -> true
  | _ -> false

let rec apply_variable_rule (value : Linast.imm_expr) (pats, (action, action_setup), guard) =
    match pats with [] -> failwith "Not Possible to have empty list"
      | p::ps ->
      (match p.pat_desc with
        | Tpat_any -> (ps, (action, action_setup), guard)
        | Tpat_var(x, _) ->
        (* TODO: Hack to avoid binding ident to itself at top of a function, remove by an optimisation pass *)
        let new_bind = match value.desc with ImmIdent i when i = x -> [] | _ -> [BLet(x, Compound.imm value)] in
        (ps, (action, new_bind @ action_setup), guard)
        | _ -> failwith "Not possible to apply variable rule")

let rec apply_mixture_rule matrix =
  let rec split_by_test test acc = function
    | [] -> acc, []
    | (((p::_, _, _) as row)::rest) as matrix -> if test p then
      split_by_test test (row::acc) rest else acc, matrix
    | _ -> failwith "Mixture rule given empty pattern, should have been picked up as success already"
      in
  match matrix with
    | ((({pat_desc=Tpat_or(_, _, _)}::_, _, _) as row)::rest) -> [row], rest
    | (((p::_, _, _) as row)::rest) -> if is_variable_pattern p then split_by_test is_variable_pattern [row] rest
      else if is_constructor_pattern p then split_by_test is_constructor_pattern [row] rest
      else failwith "Mixture rule ran into matrix it couldn't process"
    | _ -> failwith "Mixture rule ran into a matrix it couldn't process"

let rec expand_ors pat = match pat.pat_desc with
  | Tpat_or(p1, p2, _) -> (expand_ors p1) @ (expand_ors p2)
  | _ -> [pat]

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
let rec preprocess_row values ((patterns, (action, action_setup), g) as row) = match values, patterns with
  | [], [] -> row
  | v::vs, {pat_desc=Tpat_alias(p, id, _)}::ps ->
    preprocess_row values (p::ps, (action, (BLet(id, Compound.imm v))::action_setup), g)
  | v::vs, p::ps -> (match simplified_or_pattern p with None -> row
    | Some p -> preprocess_row  values (p::ps, (action, action_setup), g))
  | [], p::ps -> failwith "Malformed row, value forgotten"
  | _, _ -> failwith "malformed row"

(* Add partial information later *)
(* values is imm vector corresponding to the vector of patterns in each row *)
(* List of rows, each row is ([pattern list], ((action, action_setup), binds), (guard,setup) option)  *)
(* TODO: May need an extra rule for variants and how to process them, shouldn't be difficult *)
let rec compile_matrix fail values matrix =
  let matrix = List.map (preprocess_row values) matrix in
  match (values, matrix) with
  | (_, []) -> (Compound.fail fail, []) (* No valid patterns left *)
  | ([], _) -> include_guard fail matrix (* All values matched *)

  (* OR rule -> Compile([v], expanded_or_matrix); Compile(vs, rest_of_row) *)
  | (v::vs, [(({pat_desc=Tpat_or(_, _, _)} as p)::ps, act, g)]) ->
    let patterns = expand_ors p in
    let (or_match, or_setup) = compile_matrix fail [v]
      (List.map (fun p -> ([p], ((Compound.imm unit_value, []), []), None)) patterns) in
    let (rest, rest_setup) = compile_matrix fail vs [(ps, act, g)] in
    (rest, or_setup @ (BEffect(or_match))::rest_setup)

  (* variable rule *)
  | (v::vs, matrix) when List.for_all
      (function ([], _,_) -> failwith "Not Possible to have empty list"
              | (p::ps,_,_) -> is_variable_pattern p) matrix ->
    compile_matrix fail vs (List.map (apply_variable_rule v) matrix)

  (* mixture rule *)
  | (v::vs, matrix) when List.exists
      (function ([], _,_) -> failwith "Not Possible to have empty list"
              | (p::ps,_,_) -> not(is_constructor_pattern p)) matrix ->
     let m1, m2 = apply_mixture_rule matrix in
     let new_fail = next_fail_count () in
     let (expr1, setup1) = compile_matrix new_fail values m1 in
     let (expr2, setup2) = compile_matrix fail values m2 in
     (Compound.matchtry
        new_fail
        (binds_to_anf setup1 (LinastExpr.compound expr1))
        (binds_to_anf setup2 (LinastExpr.compound expr2)),
      [])

 (* select correct constructor rule *)
 | (v::vs, ({pat_desc=Tpat_tuple l}::_,_,_)::_) ->
    apply_tuple_rule fail v vs matrix (List.length l)

 | (v::vs, ({pat_desc=Tpat_record (_, _)}::_,_,_)::_) ->
    apply_record_rule fail v vs matrix

 | (v::vs, ({pat_desc=Tpat_array _}::_,_,_)::_) ->
    apply_array_rule fail v vs matrix

 (* Handle constant constructors specially *)
 | (v::vs, ({pat_desc=Tpat_construct (_, signature, _)}::_,_,_)::_) when signature.cstr_nonconsts = 0 ->
   let get_cstr_tag = function
      | ({pat_desc=Tpat_construct (_, desc, _)}::_,_,_) -> get_const_constructor_tag desc.cstr_tag
      | _ -> failwith "Can't apply constructor rule" in
   let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
     if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in
   (* Replace with int tags *)
   let matrix' = List.map (function (({pat_desc=Tpat_construct (_, desc, _)} as p)::ps,act,g) ->
      ({p with pat_desc=Tpat_constant(Asttypes.Const_int (get_const_constructor_tag desc.cstr_tag))}::ps, act, g)
      | _ -> failwith "Not a constructor pattern") matrix in
    apply_const_int_rule fail v vs matrix' cstrs_used (List.length cstrs_used = signature.cstr_consts)

 | (v::vs, ({pat_desc=Tpat_construct (_, signature, _)}::_,_,_)::_) ->
   (* TODO: When is cstr_normal non-zero? *)
   apply_constructor_rule fail v vs matrix (signature.cstr_consts + signature.cstr_nonconsts)

 (* TODO: May be better to use simple if/else if only 1 constant. Do in later optimisation pass? *)
 | (v::vs, ({pat_desc=Tpat_constant (Const_int _)}::_,_,_)::_) ->
    let get_const_int = function
      | ({pat_desc=Tpat_constant (Const_int i)}::_,_,_) -> i
      | _ -> failwith "Can't apply const_int rule" in
    let ints_used = List.fold_left (fun ints row -> let n = get_const_int row in
      if List.mem n ints then ints else n::ints) [] matrix in
    apply_const_int_rule fail v vs matrix ints_used false
 | (v::vs, ({pat_desc=Tpat_constant (Const_float _)}::_,_,_)::_) ->
   apply_float_rule fail v vs matrix
 | (v::vs, ({pat_desc=Tpat_constant _}::_,_,_)::_) ->raise (NotImplemented __LOC__)
 | _ -> failwith "Malformed matrix/vector input"

and apply_tuple_rule fail v vs matrix len =
  let new_val_ids = List.init len (fun _ -> Ident.create_local "tuple_arg") in
  let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
  let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
  let new_rows = List.map
  (function ({pat_desc=Tpat_tuple l}::ps,act,g) -> (l@ps, act,g) | _ -> failwith "Wrong rule applied") matrix in
  let (expr, setup) = compile_matrix fail (new_vals @ vs) new_rows in
  (expr, new_val_binds @ setup)

and apply_array_rule fail v vs matrix =
  let rec specialise_matrix n =
    let rows = List.filter (function
      | ({pat_desc=Tpat_array pl}::_,_,_) -> List.length pl = n
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | ({pat_desc=Tpat_array pl}::ps,act, g) -> (pl @ ps, act, g)
     | _ -> failwith "Wrong rule applied") rows in
    let new_val_ids = List.init n (fun _ -> Ident.create_local "array_arg") in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
    let (expr, setup) = compile_matrix fail (new_vals @ vs) new_matrix in
    (n, binds_to_anf (new_val_binds @ setup) (LinastExpr.compound expr))
    in
  let get_length = function
    | ({pat_desc=Tpat_array l}::_,_,_) -> List.length l
    | _ -> failwith "Can't apply constructor rule" in
  let lengths_used = List.fold_left (fun lens row -> let n = get_length row in
    if List.mem n lens then lens else n::lens) [] matrix in
  let cases = List.map specialise_matrix lengths_used in
  let len_id = Ident.create_local "array_len" in
  let len_imm = Imm.id len_id in
  let len_bind = BLet(len_id, Compound.gettag v) in
  (Compound.mkswitch len_imm cases (Some (LinastExpr.compound (Compound.fail fail))), [len_bind])

and apply_constructor_rule fail v vs matrix num_constructors =
  let rec specialise_constructor_matrix tag =
    let rows = List.filter (function (* Could just check equality of tag_desc's? *)
      | ({pat_desc=Tpat_construct (_, tag_desc, _)}::_, _, _) -> (unify_constructor_tag tag_desc) = tag
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | ({pat_desc=Tpat_construct (_, _, pl)}::ps,act, g) -> (pl @ ps, act, g)
     | _ -> failwith "Wrong rule applied") rows in
    let arity = match rows with ({pat_desc=Tpat_construct (_, _, pl)}::_,_,_)::_ -> List.length pl
      | _ -> failwith "Wrong rule applied" in
    let new_val_ids = List.init arity (fun _ -> Ident.create_local "cstr_arg") in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
    let (expr, setup) = compile_matrix fail (new_vals @ vs) new_matrix in
    (tag, binds_to_anf (new_val_binds @ setup) (LinastExpr.compound expr)) in

  let get_cstr_tag = function
    | ({pat_desc=Tpat_construct (_, tag_desc, _)}::_,_,_) -> unify_constructor_tag tag_desc
    | _ -> failwith "Can't apply constructor rule" in
  let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
    if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in
  let cases = List.map specialise_constructor_matrix cstrs_used in
  let tag_id = Ident.create_local "cstr_tag" in
  let tag_imm = Imm.id tag_id in
  let tag_bind = BLet(tag_id, Compound.gettag v) in
  if List.length cstrs_used = num_constructors
  then (Compound.mkswitch tag_imm cases None, [tag_bind])
  else (Compound.mkswitch tag_imm cases
    (Some (LinastExpr.compound (Compound.fail fail))), [tag_bind])

and apply_record_rule fail v vs matrix =
    let get_label_pos (_, desc, _) = desc.lbl_pos in
    let rec get_labels_used acc = function
      | ({pat_desc=Tpat_record (l, _)}::_,_,_) ->
        (* TODO: Would be nice to put these in sorted order at end *)
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
      (function ({pat_desc=Tpat_record (l, _)}::ps,act,g) ->
         ((List.map (get_pattern (make_wildcard_pattern l) l) labels_used)@ps, act,g)
        | _ -> failwith "Wrong rule applied") matrix in
    let (expr, setup) = compile_matrix fail (new_vals @ vs) new_rows in
    (expr, new_val_binds @ setup) (* Extract each of the fields then recursively match the pattern *)

(* TODO: Handle other constants *) (* Total=true when created from full constant constructor signature *)
(* Working out ints_used factored out to allow computing if total or not for const constructors *)
and apply_const_int_rule fail v vs matrix ints_used total =
  let specialise_const_int_matrix n =
    let rows = List.filter (function
      | ({pat_desc=Tpat_constant (Const_int m)}::_,_,_) -> m = n
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | (_::ps,act,g) -> (ps, act, g)
     | _ -> failwith "Wrong rule applied") rows in
    let (expr, setup) = compile_matrix fail vs new_matrix in
    (n, binds_to_anf setup (LinastExpr.compound expr)) in
  let cases = List.map specialise_const_int_matrix ints_used in
  if total then (Compound.mkswitch v cases None, []) else
  (Compound.mkswitch v cases (Some (LinastExpr.compound (Compound.fail fail))), [])

(* Can't switch on floats, so have to generate nested if-then-else *)
and apply_float_rule fail v vs matrix =
  let specialise_float_matrix f =
    let rows = List.filter (function (* All constants must be floats due to type-checking *)
      | ({pat_desc=Tpat_constant c}::_,_,_) -> f = c
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | (_::ps,act,g) -> (ps, act, g)
     | _ -> failwith "Wrong rule applied") rows in
    let (expr, setup) = compile_matrix fail vs new_matrix in
    (f, binds_to_anf setup (LinastExpr.compound expr)) in
  let get_float = function
        | ({pat_desc=Tpat_constant c}::_,_,_) -> c
        | _ -> failwith "Can't apply float rule" in
  let floats_used = List.fold_left (fun floats row -> let f = get_float row in
    if List.mem f floats then floats else f::floats) [] matrix in
  let cases = List.map specialise_float_matrix floats_used in
  (* Made awkward by wanting to return a compound *)
  List.fold_right (fun (f, body) (rest, rest_setup) ->
    let test_result = Compound.binary Eq (Imm.const f) v in
    let testid = Ident.create_local "isequal" in
    (Compound.mkif (Imm.id testid) body (binds_to_anf rest_setup (LinastExpr.compound rest)),
     [BLet(testid, test_result);])) cases (Compound.fail fail, [])
