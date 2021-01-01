(* Optimised version of pattern matching, see https://dl.acm.org/doi/pdf/10.1145/507669.507641 *)
(*
Elements that need implementing:
- Identifying if two rows are incompatible, for allowing swapping them (just ignore guards)
- Get exhaustiveness information from cases
- Additionally use contexts to have better knowledge of whether all cases are considered or not
- Track 'Context' i.e. more pattern matrices, and use these to select from a list of handlers on exits
- Can expand out OR patterns as long as no later rows compatible with OR pattern. A match triggers an
  exit and can pass out variables bound by that part of the OR pattern, so now have some scope breaking.
  Whole compilation gets wrapped in a handler for that OR pattern (see paper for exact details)

Extra arguments to main algorithm:
- exhaustive (already there, but not really used)
- Reachable trap handlers, list of (pattern matrix, handler int)
- ctx, matrix (sort of 2 matrices) P . Q of prefix P and fringe Q.
  Probably best to implement as a list of pairs of lists i.e. (row1, row1')::(row2, row2'):: ...
  Operations on contexts include pushing, popping, collection and specialisation

Extra return result:
- Jump summary mapping handler identifiers to contexts i.e. knowledge of values at each exit

Operations on contexts also need to extend to matrices (where appropriate) by treating prefix as empty.
In this way, also naturally extend to jump summaries (reachable handlers)

*)

(*
Parmatch.le_pats used to check when a row is redundant for OR patterns
Parmatch.lubs used (in matrix.ml) to check compatibility
*)


open LinastUtils
open Linast
open Typedtree
open Types
open Asttypes
open Matrix

(* Default value is -1 for trap, otherwise this is used to access an outer try/catch *)
let fail_trap = -1l

let fail_count = ref fail_trap

let next_fail_count () =
  fail_count := Int32.add 1l (!fail_count);
  !fail_count

let include_guard fail ctx ((action, action_setup), binds) = function
  | None -> ((action, binds @ action_setup), [])
  (* Guard present so return non-empty jump summary *)
  | Some (comp, guard_setup) ->
    let id = Ident.create_local "guard" in
    let id_imm = Imm.id id in
    ((Compound.mkif (id_imm) (binds_to_anf action_setup (LinastExpr.compound action)) (LinastExpr.compound (Compound.fail fail)),
     binds @ guard_setup @ [BLet(id, comp)]), [(fail, ctx)])

(* Aliases case still needed here as it is used by simplified_or_patterns i.e. before removal of aliases *)
(* TODO: Variant patterns? *)
let rec is_variable_pattern pat = match pat.pat_desc with
  | Tpat_any | Tpat_var(_, _) -> true
   | Tpat_alias(p, _, _) -> is_variable_pattern p
  | _ -> false
let is_variable_row = function
  | (p::_, _, _) -> is_variable_pattern p
  | _ -> failwith "Cannot check for variable_row on empty row"

let is_constructor_pattern pat = match pat.pat_desc with
  | Tpat_constant _ | Tpat_tuple _ | Tpat_construct(_, _, _) | Tpat_record(_, _) | Tpat_array _ -> true
  | _ -> false
let is_constructor_row = function
  | (p::_, _, _) -> is_constructor_pattern p
  | _ -> failwith "Cannot check for constructor_row on empty row"

let rec apply_variable_rule (value : Linast.imm_expr) (pats, (action, action_setup), guard) =
  match pats with
    | [] -> failwith "Not Possible to have empty list"
    | p::ps ->
      (match p.pat_desc with
        | Tpat_any -> (ps, (action, action_setup), guard)
        | Tpat_var(x, _) ->
        (* Avoid binding ident to itself at top of a function *)
          let new_bind = match value.desc with ImmIdent i when i = x -> []
            | _ -> [BLet(x, Compound.imm value)] in
          (ps, (action, new_bind @ action_setup), guard)
        | _ -> failwith "Not possible to apply variable rule")

(*
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
*)

(* if no OR pattern, returns None
   if some OR pattern, returns ([rows_above], last_of_pat, rest_of_row, [rows_below]) *)
let rec split_at_last_or_row = function
  | [] -> None
  | ((({pat_desc=Tpat_or (_, _, _)} as pat)::pats, act, g) as row)::rest ->
    (match split_at_last_or_row rest with
      | None -> Some ([], pat, (pats, act, g), rest)
      | Some (before, or_pat, or_row, after) -> Some (row::before, or_pat, or_row, after))
  | row::rest -> Option.map
   (fun (before, or_pat, or_row, after) -> (row::before, or_pat, or_row, after))
   (split_at_last_or_row rest)

(* The two conditions described in the paper.
  Effectively says that a match of the head of the OR row need not consider remaining rows after *)
(* or_row is OR pattern row with the head removed *)
let can_apply_or_rule or_pat or_row g rest =
  List.for_all (function (p::(row : Typedtree.pattern list), _, _) ->
    (* First patterns incompatible *)
    (lub_pat_opt or_pat p) = None ||
    (* Other patterns more precise and OR row can't fail due to guard.
       Later rows are still necessary as first pattern may match some values the OR pattern doesn't. *)
    (g = None && Parmatch.le_pats or_row row)
    | _ -> failwith "malformed matrix when considering or rule"
  ) rest

(* acc is the reversed list of the longest prefix of matrix to all satisfy test *)
let rec collect_rows test acc = function
  | [] -> acc, []
  | (row::rest) as matrix ->
    if test row then collect_rows test (row::acc) rest else acc, matrix
(*
 Corresponds to cases i, ii, iii on page 11, right column in paper
 Attempt to construct 3 matrices mat_C, mat_O, mat_R where:
 If mat_O empty then can just apply constructor (/variable) rule to mat_C, and split with mat_R.
 If mat_O not empty, can first apply OR rule to mat_C @ mat_O then the more basic rule, and again split with mat_R

 mat_C and mat_R kept in reverse order for efficiency,
 mat_O kept in actual order for correctness of OR rule test
*)
(* if is_or_pat, cannot move row into mat_C *)
let move_up_row is_or_pat (mat_C, mat_O, mat_R) ((pats, _, _) as row) =
  (* Incompatible with all of O and R (can assume guards always succeed) *)
  if (not is_or_pat) &&
    List.for_all (fun (other_row, _, _) -> (lub_mat_opt pats other_row) = None) (mat_O @ mat_R) then
    (row::mat_C, mat_O, mat_R)
  (* Incompatible with all of R, and adding to O allows applying OR rule *)
  else if List.for_all (fun (other_row, _, _) -> (lub_mat_opt pats other_row) = None) mat_R &&
    (match split_at_last_or_row (mat_O @ [row]) with None -> false
     | Some (before, or_pat, (pats, action, g), after) -> can_apply_or_rule or_pat pats g after) then
    (mat_C, mat_O @ [row], mat_R)
  (* Can't optimise, add to R *)
  else (mat_C, mat_O, row::mat_R)

(* Split the matrix ready for the mixture rule *)
let apply_constructor_split matrix =
  (* Initial phase is the same as unoptimised version *)
  (* Same naming of matrices used as in paper (right column page 10) *)
  (* C is the top group of constructor patterns, constructors below incompatible variable patterns get moved into it.
     O is matrix suitable for OR rule, so will apply that then the Constructor rule on C @ O
     R is rows that don't fit either of these, so cannot move above in matrix. *)
  let mat_C, mat_P' = collect_rows is_constructor_row [] matrix in
  (* Attempt to extend the matrix where cases are incompatible *)
  (* Only mat_O is kept in order, since order actually matters for applying the OR rule *)
  let mat_C, mat_O, mat_R = List.fold_left
   (fun (mat_C, mat_O, mat_R) row ->
    if is_variable_row row then mat_C, mat_O, (row::mat_R) else
    if is_constructor_row row then
      move_up_row false (mat_C, mat_O, mat_R) row
    else (* or pattern *)
       move_up_row true (mat_C, mat_O, mat_R) row
  ) (mat_C, [], []) mat_P' in
  (List.rev mat_C) @ mat_O, (List.rev mat_R)

(* as above but variable/constructor swapped *)
let apply_variable_split matrix =
  let mat_C, mat_P' = collect_rows is_variable_row [] matrix in
  (* Attempt to extend the matrix where cases are incompatible *)
  (* Only mat_O is kept in order, since order actually matters for applying the OR rule *)
  let mat_C, mat_O, mat_R = List.fold_left
   (fun (mat_C, mat_O, mat_R) row ->
    if is_constructor_row row then mat_C, mat_O, (row::mat_R) else
    if is_variable_row row then
      move_up_row false (mat_C, mat_O, mat_R) row
    else (* or pattern *)
       move_up_row true (mat_C, mat_O, mat_R) row
  ) (mat_C, [], []) mat_P' in
  (List.rev mat_C) @ mat_O, (List.rev mat_R)

(* Original pattern matching approach always separated OR patterns out on their own.
   Instead, look for longest prefix OR rule can apply to by trying each one (naive search).
   Like the two functions above but with C always empty *)
(* Tests on the head of the first row select which function to call. So this one should only be
   called if it can actually produce a useful matrix *)
let apply_or_split matrix =
  (* mat_C kept to reduce number of things needing to be re-declared *)
  let mat_C, mat_O, mat_R = List.fold_left (move_up_row true) ([], [], []) matrix in
  mat_O, (List.rev mat_R)

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

(* TODO: Worth testing for redudant cases or not? *)
(* For the first column, rewrite any aliases to just happen in the action, and simplify OR patterns *)
let rec preprocess_row values ((patterns, (action, action_setup), g) as row) = match values, patterns with
  | [], [] -> row
  | v::vs, {pat_desc=Tpat_alias(p, id, _)}::ps ->
    preprocess_row values (p::ps, (action, (BLet(id, Compound.imm v))::action_setup), g)
  | v::vs, p::ps -> (match simplified_or_pattern p with None -> row
    | Some p -> preprocess_row  values (p::ps, (action, action_setup), g))
  | [], p::ps -> failwith "Malformed row, value forgotten"
  | _, _ -> failwith "malformed row"

(* List of rows, each row is ([pattern list], ((action, action_setup), binds), (guard,setup) option)  *)
(* TODO: May need an extra rule for variants and how to process them, shouldn't be difficult *)
(* TODO: Encode fact that variables vector/length of row should match up by putting inside a
         structure with special getter functions, so only see exceptions in 1 place *)
(* TODO: Check that, whenever head of handler list is gotten, it must actually exist *)
(* TODO: Where is swapping of incompatible rows done in mixture rule? *)
let rec compile_matrix values matrix total handlers ctx =
  let matrix = List.map (preprocess_row values) matrix in
  match (values, matrix) with
  (* Besides the fail case here, have 5 rules to consider, as described in paper *)
  | (_, []) -> (* When does this occur? Take innermost handler *)
    let _, fail_idx = List.hd handlers in
    (* TODO: Work out what this should actually generate, needs to indicate handler taken? *)
    ((Compound.fail fail_idx, []), [(fail_idx, ctx)]) (* No valid patterns left *)

  (* Case 1. Successful match. Whether jump summary needed depends on
     if guard is present or not, jump summary is returned by include_guard *)
  | ([], ([], act, g)::rest) ->
    let _, fail_idx = List.hd handlers in
    include_guard fail_idx ctx act g

  (* Case 2. Variable rule. Only change is correctly handling total information, handlers and context *)
  | (v::vs, matrix) when List.for_all
      (function ([], _,_) -> failwith "Not Possible to have empty list"
              | (p::ps,_,_) -> is_variable_pattern p) matrix ->
    let code, jump_summary =
      compile_matrix vs (List.map (apply_variable_rule v) matrix)
        total (push_handlers handlers) (push_ctx ctx) in
    code, pop_jump_summary jump_summary

  (* Case 3. Constructor rule. TODO: Handle pseudo-constructors i.e. constant/tuple/record/array *)
  (* TODO: Also handle constant constructors specially i.e. cstr_nonconst != 0 *)
  | (v::vs, ({pat_desc=Tpat_construct (_, signature, _)}::_,_,_)::_)
    when List.for_all (function ([], _,_) -> failwith "Not Possible to have empty list"
                       | (p::ps,_,_) -> is_constructor_pattern p) matrix
    && signature.cstr_nonconsts = 0 -> failwith "Constant constructors not implemented yet"

  | (v::vs, (({pat_desc=Tpat_construct (_, signature, _)} as pat)::_,_,_)::_)
    when List.for_all (function ([], _,_) -> failwith "Not Possible to have empty list"
                       | (p::ps,_,_) -> is_constructor_pattern p) matrix ->
     apply_constructor_rule total handlers ctx v vs matrix signature pat

  (* p pattern only used for mixture rule (case 5) to determine process for splitting up matrix *)
  | (v::vs, (p::_, _, _)::_) ->
    (match split_at_last_or_row matrix with
    (* Case 4. OR pattern. let row i be a row starting with an OR pattern.
     We require that either
       a) Every row after i is more precise than i, so is unused anyway (row i must not have guard)
       b) Every row after i is incompatible with i
     Can then split up matrix accordingly.
     Any binds made in matching the OR'd pattern will escape scoping when used! *)
    (* Choice of which OR to split on doesn't change if matrix can be compiled without mix rule or not *)
    | Some (before, or_pat, ((pats, action, g) as rest_of_row), after) when
      can_apply_or_rule or_pat pats g after ->
      let patterns = expand_ors or_pat in
      let new_fail = next_fail_count () in
      let new_rows = List.map
        (fun pat -> (pat :: (omegas (List.length pats)), ((Compound.fail new_fail, []), []), None))
        patterns in
      (* First recursive call. Original matrix but with OR row expanded to alway exit *)
      let (rest_comp, rest_setup), rest_jumps =
        (* Only difference to case b is that the 'after' rows still need to be compiled *)
        compile_matrix values (before @ new_rows @ after) total handlers ctx in
      (* Remaining patterns of the OR row in the event it matches *)
      (* TODO: Check that variables escaping scoping still work correctly here, particularly guards *)
      let (or_comp, or_setup), or_jumps = compile_matrix vs [rest_of_row] total
        (push_handlers (extract_handlers or_pat handlers))
        (push_ctx (extract_ctx or_pat ctx)) in
      ((Compound.matchtry new_fail
          (binds_to_anf rest_setup (LinastExpr.compound rest_comp))
          (binds_to_anf or_setup (LinastExpr.compound or_comp)),
        []),
       union_jump_summary rest_jumps (pop_jump_summary or_jumps))

    (* Case 5, no other rule can be applied so must split matrix up into submatrices that can be processed by rules 1-4 *)
    | _ ->
      (* Use greedy approach from paper to split into an 'upper' and 'lower' matrix *)
      let upper, lower =
        if is_variable_pattern p then apply_variable_split matrix
        else if is_constructor_pattern p then apply_constructor_split matrix
        else apply_or_split matrix in

      (* Combine the two matrices *)
      let new_fail = next_fail_count () in
      (* Always partial, adds an extra jump handler for the left out cases *)
      let (upper_compound, upper_setup), upper_jumps =
        compile_matrix values upper false ((get_just_patterns lower, new_fail)::handlers) ctx in
      (* TODO: Verify that the handler will always be present in the jump summary. If not present,
               suggests that the lower half of the matrix isn't actually necessary *)
      (* Uses the context generated by all parts of compiling upper matrix which can fail to this handler *)
      let (lower_compound, lower_setup), lower_jumps =
        compile_matrix values lower total handlers (List.assoc new_fail upper_jumps) in
      ((Compound.matchtry new_fail
         (binds_to_anf upper_setup (LinastExpr.compound upper_compound))
         (binds_to_anf lower_setup (LinastExpr.compound lower_compound)),
       []),
       union_jump_summary (List.remove_assoc new_fail upper_jumps) lower_jumps
      ))
  | _ -> failwith "Malformed value vector/pattern matrix"

  (*
  (* mixture rule *)
  | (v::vs, matrix) when List.exists
      (function ([], _,_) -> failwith "Not Possible to have empty list"
              | (p::ps,_,_) -> not(is_constructor_pattern p)) matrix ->
     ...

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
 *)

(*
and apply_tuple_rule fail v vs matrix len =
  let new_val_ids = List.init len (fun _ -> Ident.create_local "tuple_arg") in
  let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
  let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
  let new_rows = List.map
  (function ({pat_desc=Tpat_tuple l}::ps,act,g) -> (l@ps, act,g) | _ -> failwith "Wrong rule applied") matrix in
  let (expr, setup) = compile_matrix fail (new_vals @ vs) new_rows in
  (expr, new_val_binds @ setup)
*)

(*
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
*)

(* Example_pat needed for providing a copy of the environment to lookup missing constructors *)
and apply_constructor_rule total handlers ctx v vs matrix signature example_pat =
  (* Now returns (tag * case body * jump_summary *)
  let rec specialise_constructor_matrix tag_desc =
    let rows = List.filter (function (* Could just check equality of tag_desc's? *)
      | ({pat_desc=Tpat_construct (_, desc, _)}::_, _, _) ->
        desc.cstr_tag = tag_desc.cstr_tag
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | ({pat_desc=Tpat_construct (_, _, pl)}::ps,act, g) -> (pl @ ps, act, g)
     | _ -> failwith "Wrong rule applied") rows in
    let arity = match rows with ({pat_desc=Tpat_construct (_, _, pl)}::_,_,_)::_ -> List.length pl
      | _ -> failwith "Wrong rule applied" in
    let new_val_ids = List.init arity (fun _ -> Ident.create_local "cstr_arg") in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
    let ((expr, setup), jumps) = compile_matrix (new_vals @ vs) new_matrix total
       (specialise_handlers tag_desc handlers) (specialise_ctx tag_desc ctx) in
    (unify_constructor_tag tag_desc,
     binds_to_anf (new_val_binds @ setup) (LinastExpr.compound expr),
     jumps) in

  let get_cstr_tag = function
    | ({pat_desc=Tpat_construct (_, tag_desc, _)}::_,_,_) -> tag_desc
    | _ -> failwith "Can't apply constructor rule" in
  (* cstrs_used changed to be the actual constructor description, not int tag *)
  let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
    if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in
  (* (int * linast * jump_summary) list *)
  let specialised_calls = List.map specialise_constructor_matrix cstrs_used in
  let tag_id = Ident.create_local "cstr_tag" in
  let tag_imm = Imm.id tag_id in
  let tag_bind = BLet(tag_id, Compound.gettag v) in

  let cases = List.map (fun (i, body, _) -> (i, body)) specialised_calls in
  let jump_summaries = List.map (fun (_, _, jumps) -> jumps) specialised_calls in
  (* Check semantics correct *)
  (* TODO: Either in union code or here, reduce all contexts to max 32 rows *)
  let summary = List.fold_right
    (fun jumps result -> union_jump_summary result (collect_jump_summary jumps))
    jump_summaries [] in

  let num_constructors = (signature.cstr_consts + signature.cstr_nonconsts) in
  if List.length cstrs_used = num_constructors || total
  then
    (* Case Total - can easily construct the result *)
    ((Compound.mkswitch tag_imm cases None, [tag_bind]), summary)

  (* 5 chosen arbitrarily as number of extra cases to allow including explicitly *)
  else if List.length cstrs_used + 5 >= num_constructors
  then
    (* Case only a few constructors missing. Add in an exit to the earliest possible handler for each *)
    let all_constructors = lookup_constructors signature example_pat in
    let unused = List.filter
      (fun desc -> List.for_all (fun tag -> tag.cstr_tag <> desc.cstr_tag) cstrs_used)
      all_constructors in

    (* Describes knowledge of the pattern being matched for each missing constructor *)
    let other_contexts = List.map
      (fun cstr_desc -> cstr_desc, extract_ctx (make_constructor_pattern example_pat cstr_desc) ctx)
      unused in
    (*
      used_handlers tracks for each i, which pattern describes the cases that go to it
      i.e. (c1'(_,...)|c2'(_,...)|...)
      Jump summary then uses this to describe the set of values which go to that handler
      Possible for no handler to match the constructor being considered. In that case, the
      constructor never actually occurs at this point in the code, so we can safely ignore it
    *)
    let other_cases, used_handlers = List.fold_right
       (fun (cstr, ctx) (cases, used_handlers) ->
         (* [] is empty context i.e. never matches, so take first non-empty one *)
         match List.find_opt
             (fun (handler_mat, i) -> intersect_matrix handler_mat (ctx_to_mat ctx) <> [])
             handlers with
           (* this constructor is guarenteed to never occur *)
           | None -> (cases, used_handlers)
           (* Exit to the first handler which could possibly handle this constructor *)
           | Some (_, i) ->
             (* Store that this constructor can cause an exit to handler i *)
             let new_used_handlers = match List.assoc_opt i used_handlers with
               | None -> (i, make_constructor_pattern example_pat cstr)::used_handlers
               | Some pat ->
                 (i, make_or_pattern pat (make_constructor_pattern example_pat cstr))::
                 (List.remove_assoc i used_handlers) in
             ((unify_constructor_tag cstr, LinastExpr.compound (Compound.fail i))::cases,
              new_used_handlers))
      other_contexts ([], []) in

      ((Compound.mkswitch tag_imm (cases @ other_cases) None, [tag_bind]),
       List.fold_right union_jump_summary
         (List.map (fun (i, pat) -> [(i, extract_ctx pat ctx)]) used_handlers)
         summary)
  else
    (* Don't try to match any of the missing patterns, just add in a default case *)
    (* Could still specialise for constructors which occur in 1st column of handler matrices
       but not in the matrix (quite a special case, unlikely to see much improvement) *)
    let _, fail_idx = List.hd handlers in
    ((Compound.mkswitch tag_imm cases
      (Some (LinastExpr.compound (Compound.fail fail_idx))), [tag_bind]),
     union_jump_summary summary [(fail_idx, ctx)])

(*
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
*)

(*
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
*)

(*
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
*)

