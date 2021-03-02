(* Same process as compileMatch but returns a Linast rather than a compound expression
   (i.e. for compiling function body vs let binding), so actions in matrix are slightly different
   Functions taken from compileMatchOpt wherever possible *)
(*
  Matrix rows are (pattern list * linast_expr) list
  Only used for simple curried args of functions, or for top-level let bindings, so no guards
 *)
(* TODO: In many places, no need to pass ~exported to binds_to_anf as it is an internally generated temporary,
         so can't possibly be exported. Should only matter for the variable rule. *)
open LinastUtils
open Linast
open Typedtree
open Types
open Asttypes
open Asttypes
open Matrix

(* Simpler than in compileMatchOpt as there are no guards, and actions are linast_exprs *)
type action_matrix =
  (pattern list * linast_expr) list

type state = {
  values : imm_expr list;
  matrix : action_matrix;
  total : bool;
  handlers : jump_handlers;
  ctx : context;
  exported : Ident.t list; (* since compileLet is responsible for top-level let expressions *)
}

let is_variable_row = function
  | (p::_, _) -> CompileMatchOpt.is_variable_pattern p
  | _ -> failwith "Cannot check for variable_row on empty row"
let is_constructor_row = function
  | (p::_, _) ->  CompileMatchOpt.is_constructor_pattern p
  | _ -> failwith "Cannot check for constructor_row on empty row"

let rec apply_variable_rule exported (value : Linast.imm_expr) (pats, action) =
  match pats with [] -> failwith "Not Possible to have empty list"
    | p::ps ->
    (match p.pat_desc with
      | Tpat_any -> (ps, action)
      | Tpat_var(x, _) ->
      let new_bind = match value.i_desc with ImmIdent i when i = x -> []
        | _ -> [BLet(x, Compound.imm value)] in
      (ps, binds_to_anf ~exported new_bind action)
      | _ -> failwith "Not possible to apply variable rule")

(* if no OR pattern, returns None
   if some OR pattern, returns ([rows_above], last_of_pat, rest_of_row, [rows_below]) *)
let rec split_at_last_or_row = function
  | [] -> None
  | ((({pat_desc=Tpat_or (_, _, _)} as pat)::pats, act) as row)::rest ->
    (match split_at_last_or_row rest with
      | None -> Some ([], pat, (pats, act), rest)
      | Some (before, or_pat, or_row, after) -> Some (row::before, or_pat, or_row, after))
  | row::rest -> Option.map
   (fun (before, or_pat, or_row, after) -> (row::before, or_pat, or_row, after))
   (split_at_last_or_row rest)

(* The two conditions described in the paper.
  Effectively says that a match of the head of the OR row need not consider remaining rows after *)
(* or_row is OR pattern row with the head removed *)
let can_apply_or_rule or_pat or_row rest =
  List.for_all (function (p::(row : Typedtree.pattern list), _) ->
    (* First patterns incompatible *)
    (lub_pat_opt or_pat p) = None ||
    (* Other patterns more precise.
      Later rows are still necessary as first pattern may match some values the OR pattern doesn't. *)
    (Parmatch.le_pats or_row row)
    | _ -> failwith "malformed matrix when considering or rule"
  ) rest

let move_up_row is_or_pat (mat_C, mat_O, mat_R) ((pats, _) as row) =
  (* Incompatible with all of O and R (can assume guards always succeed) *)
  if (not is_or_pat) &&
    List.for_all (fun (other_row, _) -> (lub_mat_opt pats other_row) = None) (mat_O @ mat_R) then
    (row::mat_C, mat_O, mat_R)
  (* Incompatible with all of R, and adding to O allows applying OR rule *)
  else if List.for_all (fun (other_row, _) -> (lub_mat_opt pats other_row) = None) mat_R &&
    (match split_at_last_or_row (mat_O @ [row]) with None -> false
     | Some (before, or_pat, (pats, action), after) -> can_apply_or_rule or_pat pats after) then
    (mat_C, mat_O @ [row], mat_R)
  (* Can't optimise, add to R *)
  else (mat_C, mat_O, row::mat_R)

(* Split the matrix ready for the mixture rule *)
let apply_constructor_split matrix =
  let mat_C, mat_P' = CompileMatchOpt.collect_rows is_constructor_row [] matrix in
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
  let mat_C, mat_P' = CompileMatchOpt.collect_rows is_variable_row [] matrix in
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

let apply_or_split matrix =
  (* mat_C kept to reduce number of things needing to be re-declared *)
  let mat_C, mat_O, mat_R = List.fold_left (move_up_row true) ([], [], []) matrix in
  mat_O, (List.rev mat_R)

(* When only a few constructors missing, add those to the list of cases and exit to the nearest possible handler.
   General enough to handle both regular constructors and const constructors which are actually ints. *)
(*  other_contexts is (tag, pattern, context) list for each missing case *)
let add_missing_cases switch_value value_setup cases jump_summary {handlers; ctx; exported} other_contexts =
  let other_cases, used_handlers = List.fold_right
     (fun (cstr, cstr_pat, ctx) (cases, used_handlers) ->
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
             | None -> (i, cstr_pat)::used_handlers
             | Some pat ->
               (i, make_or_pattern pat (cstr_pat))::
               (List.remove_assoc i used_handlers) in
           ((cstr, LinastExpr.compound (Compound.fail i))::cases,
            new_used_handlers))
    other_contexts ([], []) in
  (binds_to_anf ~exported value_setup
    (LinastExpr.compound (Compound.mkswitch switch_value (cases @ other_cases) None)),
   List.fold_right union_jump_summary
     (List.map (fun (i, pat) -> [(i, extract_ctx pat ctx)]) used_handlers) jump_summary)

(* In some situations, could still add cases for constructors which appear in first column
   of handlers, but very rarely will this be a useful optimisation. *)
let add_default_case switch_value value_setup cases jump_summary {handlers; ctx; exported} =
    let _, fail_idx = List.hd handlers in
    (binds_to_anf ~exported value_setup (LinastExpr.compound (Compound.mkswitch switch_value cases
      (Some (LinastExpr.compound (Compound.fail fail_idx))))),
     union_jump_summary jump_summary [(fail_idx, ctx)])

let rec preprocess_row exported values ((patterns, act) as row) = match values, patterns with
  | [], [] -> row
  | v::vs, {pat_desc=Tpat_alias(p, id, _)}::ps ->
    preprocess_row exported values (p::ps, binds_to_anf ~exported [BLet(id, Compound.imm v)] act)
  | v::vs, p::ps -> (match CompileMatchOpt.simplified_or_pattern p with None -> row
    | Some p -> preprocess_row exported values (p::ps, act))
  | [], p::ps -> failwith "Malformed row, value forgotten"
  | _, _ -> failwith "malformed row"


let rec compile_matrix ({values; matrix; total; handlers; ctx; exported} as state) =
  let matrix = List.map (preprocess_row exported values) matrix in
  (* update with preprocessed row, so no alias patterns present *)
  let state = {state with matrix} in
  match (values, matrix) with
  (* Besides the fail case here, have 5 rules to consider, as described in paper *)
  | (_, []) ->
    let _, fail_idx = List.hd handlers in
    (LinastExpr.compound (Compound.fail fail_idx), [(fail_idx, ctx)]) (* No valid patterns left *)

  (* Case 1. Successful match. No guards so return top action *)
  | ([], ([], act)::_) -> act, []

  (* Case 2. Variable rule. Only change is correctly handling total information, handlers and context *)
  | (v::vs, matrix) when List.for_all
      (function ([],_) -> failwith "Not Possible to have empty list"
              | (p::ps,_) -> CompileMatchOpt.is_variable_pattern p) matrix ->
    let code, jump_summary =
      compile_matrix {values=vs; matrix=List.map (apply_variable_rule exported v) matrix; total;
        handlers=push_handlers handlers; ctx=push_ctx ctx; exported} in
    code, pop_jump_summary jump_summary

  (* case 4 or 5 i.e. not the constructor rule *)
  (* p pattern only used for mixture rule (case 5) to determine process for splitting up matrix *)
  | (v::vs, (p::_, _)::_) when List.exists
    (function ([],_) -> failwith "Not Possible to have empty list"
     | (p::ps,_) -> not(CompileMatchOpt.is_constructor_pattern p)) matrix ->
    (match split_at_last_or_row matrix with
    (* Case 4. OR pattern. let row i be a row starting with an OR pattern.
     We require that either
       a) Every row after i is more precise than i, so is unused anyway (row i must not have guard)
       b) Every row after i is incompatible with i
     Can then split up matrix accordingly.
     Any binds made in matching the OR'd pattern will escape scoping when used! *)
    (* Choice of which OR to split on doesn't change if matrix can be compiled without mix rule or not *)
    | Some (before, or_pat, ((pats, action) as rest_of_row), after) when
      can_apply_or_rule or_pat pats after ->
      let patterns = CompileMatchOpt.expand_ors or_pat in
      let new_fail = CompileMatchOpt.next_fail_count () in
      let new_rows = List.map
        (fun pat -> (pat :: (omegas (List.length pats)), LinastExpr.compound (Compound.fail new_fail)))
        patterns in
      (* First recursive call. Original matrix but with OR row expanded to alway exit *)
      let rest_code, rest_jumps =
        (* Only difference to case b is that the 'after' rows still need to be compiled *)
        compile_matrix {state with matrix=before @ new_rows @ after} in
      (* Remaining patterns of the OR row in the event it matches *)
      let or_code, or_jumps =
        compile_matrix {values=vs; matrix=[rest_of_row]; total; exported;
          handlers=push_handlers (extract_handlers or_pat handlers); ctx=push_ctx (extract_ctx or_pat ctx)} in
      (LinastExpr.compound (Compound.matchtry new_fail rest_code or_code),
       union_jump_summary rest_jumps (pop_jump_summary or_jumps))

    (* Case 5, no other rule can be applied so must split matrix up into submatrices that can be processed by rules 1-4 *)
    | _ ->
      (* split into enough submatrices that each can be compiled without additional immediate use of mixture rule *)
      let rec split_matrix acc = function
        | [] -> acc
        | (([], _)::_) -> failwith "Mixture rule given empty matrix to split"
        | ((p::_, _)::_) as matrix ->
          (* Use greedy approach from paper to split into an 'upper' and 'lower' matrix *)
          let upper, lower =
            if CompileMatchOpt.is_variable_pattern p then apply_variable_split matrix
            else if CompileMatchOpt.is_constructor_pattern p then apply_constructor_split matrix
            else apply_or_split matrix in
          split_matrix (upper::acc) lower in

      let submatrices = split_matrix [] matrix in

      (* Build bottom-up so that handlers are nested and as many are visible as possible for shortcutting fails *)
      (* list of submatrices given in reverse order, to get nested handlers to allow shortcutting on fails *)
      let rec apply_rule state = function
        | [] -> failwith "Mixture rule given empty matrix to split"
        | [m] -> compile_matrix {state with matrix=m}
        | lower::rest ->
          (* compile all higher sub-matrices, accumulating handlers *)
          let new_fail = CompileMatchOpt.next_fail_count () in
          let new_handlers = (List.map (fun (pats, _) -> pats) lower, new_fail)::state.handlers in
          let upper_code, upper_jumps =
            apply_rule {state with handlers=new_handlers; total=false} rest in

          (* compile lower part of matrix with context gotten from jump summary *)
          let lower_code, lower_jumps =
            compile_matrix {state with matrix=lower; ctx=List.assoc new_fail upper_jumps} in

          (* Combine the two results *)
          (LinastExpr.compound (Compound.matchtry new_fail upper_code lower_code),
             union_jump_summary (List.remove_assoc new_fail upper_jumps) lower_jumps) in

      apply_rule state submatrices
    )

  (* Case 3. Constructor rule. *)
  (* Remaining patterns determine which constructor rule to apply.
     Guarenteed that each row starts with a constructor pattern *)
  | (v::vs, ({pat_desc=Tpat_construct (_, signature, _)}::_,_)::_)
    when signature.cstr_nonconsts = 0 ->
    (* Replace each constructor in the first column with the int it's represented as *)
    let matrix' = List.map (function (({pat_desc=Tpat_construct (_, desc, _)} as p)::ps,act) ->
      ({p with pat_desc=Tpat_constant(Const_int (get_const_constructor_tag desc.cstr_tag))}::ps, act)
      | _ -> failwith "Not a constructor pattern") matrix in
    apply_const_int_rule v {state with values=vs; matrix=matrix'} (Some signature.cstr_consts)
  (* Actual constructor *)
  | (v::vs, (({pat_desc=Tpat_construct (_, signature, _)} as pat)::_,_)::_) ->
     apply_constructor_rule v {state with values=vs} signature pat
  (* Tuple *)
  | (v::vs, ({pat_desc=Tpat_tuple l}::_,_)::_) ->
    apply_tuple_rule v {state with values=vs} (List.length l)

  (* Can't handle float and integer constants together, since switch statements only work for ints *)
  | (v::vs, ({pat_desc=Tpat_constant (Const_int _)}::_,_)::_) ->
    apply_const_int_rule v {state with values=vs} None
  | (v::vs, ({pat_desc=Tpat_constant (Const_float _)}::_,_)::_) ->
    apply_float_rule v {state with values=vs}
  | (v::vs, ({pat_desc=Tpat_constant _}::_,_)::_) -> raise (NotImplemented __LOC__)
  | (v::vs, ({pat_desc=Tpat_array _}::_,_)::_) ->
    apply_array_rule v {state with values=vs}
  | (v::vs, ({pat_desc=Tpat_record (_, _)}::_,_)::_) ->
    apply_record_rule v {state with values=vs}
  | _ -> failwith "Malformed value vector/pattern matrix"

(* Tuple case is unchanged, simple since the tuple type only has one 'constructor' () *)
and apply_tuple_rule v {values=vs; matrix; total; handlers; ctx; exported} arity =
  let new_val_ids = List.init arity (fun _ -> Ident.create_local "tuple_arg") in
  let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
  let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
  let new_matrix = List.map
  (function ({pat_desc=Tpat_tuple l}::ps, act) -> (l@ps, act) | _ -> failwith "Can't apply tuple rule") matrix in
  let code, jumps =
    compile_matrix {values=new_vals @ vs; matrix=new_matrix; total; exported;
      handlers=specialise_handlers (Tuple arity) handlers; ctx=specialise_ctx (Tuple arity) ctx} in
  binds_to_anf ~exported new_val_binds code, (collect_jump_summary jumps)

(* Code otherwise duplicated across constructor/int/float/array rules.
   Filter out required rows, modify each row, make recursive call, return (tag, result, jump_summary) *)
(* Second row of parameters identify the parts specific to each type of constructor *)
and specialise_constructor_matrix v {values=vs; matrix; total; handlers; ctx; exported}
  filter row_mapper num_fields specialisation tag =
    let rows = List.filter filter matrix in
    let new_matrix = List.map row_mapper rows in
    let new_val_ids = List.init num_fields (fun _ -> Ident.create_local "cstr_field") in
    let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
    let new_val_binds = List.mapi (fun i id -> BLet(id, Compound.field v i)) new_val_ids in
    let (code, jumps) =
      compile_matrix {values=new_vals @ vs; matrix=new_matrix; total; exported;
        handlers=specialise_handlers specialisation handlers; ctx=specialise_ctx specialisation ctx} in
    (tag, binds_to_anf ~exported new_val_binds code), jumps

and apply_constructor_rule v ({values=vs; matrix; total; handlers; ctx; exported} as state) signature example_pat =
  let get_cstr_tag = function
    | ({pat_desc=Tpat_construct (_, tag_desc, _)}::_,_) -> tag_desc
    | _ -> failwith "Can't apply constructor rule" in
  (* cstrs_used changed to be the actual constructor description, not int tag *)
  let cstrs_used = List.fold_left (fun cstrs row -> let tag = get_cstr_tag row in
    if List.mem tag cstrs then cstrs else tag::cstrs) [] matrix in

  (* (int * linast * jump_summary) list *)
  let specialised_calls = List.map
    (fun cstr ->
      let filter =
        (function | ({pat_desc=Tpat_construct (_, desc, _)}::_, _) -> desc.cstr_tag = cstr.cstr_tag
                  | _ -> failwith "Wrong rule applied") in
      let row_mapper =
          (function | ({pat_desc=Tpat_construct (_, _, pl)}::ps,act) -> (pl @ ps, act)
                    | _ -> failwith "Wrong rule applied") in
     specialise_constructor_matrix v state
       filter row_mapper cstr.cstr_arity (Construct cstr) (unify_constructor_tag cstr))
     cstrs_used in

  let tag_id = Ident.create_local "cstr_tag" in
  let tag_imm = Imm.id tag_id in
  let tag_bind = BLet(tag_id, Compound.gettag v) in

  let cases, jump_summaries = List.split specialised_calls in
  (* Check semantics correct *)
  let summary = List.fold_right
    (fun jumps result -> union_jump_summary result (collect_jump_summary jumps))
    jump_summaries [] in

  let num_constructors = (signature.cstr_consts + signature.cstr_nonconsts) in
  if List.length cstrs_used = num_constructors || total
  then
    (* Case Total - can easily construct the result *)
    (binds_to_anf ~exported [tag_bind] (LinastExpr.compound (Compound.mkswitch tag_imm cases None)), summary)

  (* 5 chosen arbitrarily as number of extra cases to allow including explicitly *)
  else if List.length cstrs_used + 5 >= num_constructors
  then
    (* Case only a few constructors missing. Add in an exit to the earliest possible handler for each *)
    let all_constructors = lookup_constructors signature example_pat in
    let unused = List.filter
      (fun desc -> List.for_all (fun tag -> tag.cstr_tag <> desc.cstr_tag) cstrs_used)
      all_constructors in
    (* Describes knowledge of the pattern being matched for each missing constructor *)
    (* list of (tag, pattern, context) for each missing constructor *)
    let other_contexts = List.map
      (fun cstr_desc -> unify_constructor_tag cstr_desc,
        make_constructor_pattern example_pat cstr_desc,
        extract_ctx (make_constructor_pattern example_pat cstr_desc) ctx)
      unused in
    add_missing_cases tag_imm [tag_bind] cases summary state other_contexts
  else
    add_default_case tag_imm [tag_bind] cases summary state

(* Handles both regular const ints (maximum = None) and const constructors (maximum = Some num_constrs).
   For constant constructors, patterns are rewritten to use Const_int before this is called *)
and apply_const_int_rule v ({values=vs; matrix; total; handlers; ctx; exported} as state) maximum =
  let get_const_int = function
    | ({pat_desc=Tpat_constant (Const_int i)}::_,_) -> i
    | _ -> failwith "Can't apply const_int rule" in
  let ints_used = List.fold_left (fun ints row -> let n = get_const_int row in
    if List.mem n ints then ints else n::ints) [] matrix in

  (* (int * linast * jump_summary) list *)
  let specialised_calls = List.map
    (fun n ->
      let filter =
        (function | ({pat_desc=Tpat_constant (Const_int m)}::_,_) -> m = n
                  | _ -> failwith "Wrong rule applied") in
      let row_mapper =
          (function | (_::ps,act) -> (ps, act)
                    | _ -> failwith "Wrong rule applied") in
     specialise_constructor_matrix v state
       filter row_mapper 0 (Constant (Const_int n)) n)
     ints_used in

  let cases, jump_summaries = List.split specialised_calls in
  let summary = List.fold_right
    (fun jumps result -> union_jump_summary result (collect_jump_summary jumps))
    jump_summaries [] in

  if maximum = None
  then (* regular int case *)
    add_default_case v [] cases summary state
  else (* const constructor case *)
    let num_constrs = Option.get maximum in
    if List.length ints_used = num_constrs || total
    then
      (* Case Total - can easily construct the result *)
      (LinastExpr.compound (Compound.mkswitch v cases None), summary)
    (* 5 chosen arbitrarily as number of extra cases to allow including explicitly *)
    else if List.length ints_used + 5 >= num_constrs
    then
      (* Case only a few constructors missing. Add in an exit to the earliest possible handler for each *)
      let all_ints = List.init num_constrs (fun i -> i) in
      let unused = List.filter
        (fun i -> List.for_all (fun j -> i <> j) ints_used)
        all_ints in

      (* Describes knowledge of the pattern being matched for each missing constructor *)
      let other_contexts = List.map
        (fun i -> i,
           add_dummy_data (Tpat_constant (Const_int i)),
           extract_ctx (add_dummy_data (Tpat_constant (Const_int i))) ctx)
        unused in
      add_missing_cases v [] cases summary state other_contexts
    else
      add_default_case v [] cases summary state

(* Like constructor rule but never exhaustive, and variant is just the length of the array *)
and apply_array_rule v state =
  let get_length = function
    | ({pat_desc=Tpat_array l}::_,_) -> List.length l
    | _ -> failwith "Can't apply constructor rule" in
  let lengths_used = List.fold_left (fun lens row -> let n = get_length row in
      if List.mem n lens then lens else n::lens) [] state.matrix in

  (* (int * linast * jump_summary) list *)
  let specialised_calls = List.map
    (fun n ->
      let filter =
        (function | ({pat_desc=Tpat_array pl}::_,_) -> List.length pl = n
                  | _ -> failwith "Wrong rule applied") in
      let row_mapper =
          (function | ({pat_desc=Tpat_array pl}::ps,act) -> (pl @ ps, act)
                    | _ -> failwith "Wrong rule applied") in
     specialise_constructor_matrix v state
       filter row_mapper n (Array n) n)
     lengths_used in

  let cases, jump_summaries = List.split specialised_calls in
  let len_id = Ident.create_local "array_len" in
  let len_imm = Imm.id len_id in
  let len_bind = BLet(len_id, Compound.gettag v) in
  let summary = List.fold_right
    (fun jumps result -> union_jump_summary result (collect_jump_summary jumps))
    jump_summaries [] in
  (* Can never match every array length *)
  add_default_case len_imm [len_bind] cases summary state

and apply_record_rule v {values=vs; matrix; total; handlers; ctx; exported} =
  let rec get_labels_used acc = function
    | ({pat_desc=Tpat_record (l, _)}::_,_) ->
      (* TODO: Would be nice to put these in sorted order at end *)
      List.fold_left (fun acc (_, desc, _) ->
      if (List.find_opt (fun lbl -> lbl.lbl_pos = desc.lbl_pos) acc = None)
      then desc::acc else acc) acc l
    | _ -> failwith "Can't apply record rule" in
  (* Updated to be an actual list of labels, not just positions *)
  let labels_used = List.fold_left get_labels_used [] matrix in
  let new_val_ids = List.map (fun _ -> Ident.create_local "record_field") labels_used in
  let new_vals = List.map (fun id -> Imm.id id) new_val_ids in
  let new_val_binds = List.map (fun (desc, id) -> BLet(id, Compound.field v desc.lbl_pos))
   (List.combine labels_used new_val_ids) in

  let new_matrix = List.map (* Each Tpat_record is replaced with a pattern for each label examined *)
    (function ({pat_desc=Tpat_record (l, _)}::ps,act) ->
       ((get_record_patterns l labels_used) @ ps, act)
      | _ -> failwith "Wrong rule applied") matrix in
  let code, jumps =
    compile_matrix {values=new_vals @ vs; matrix=new_matrix; total; exported;
      handlers=specialise_handlers (Record labels_used) handlers; ctx=specialise_ctx (Record labels_used) ctx} in
  (* Like tuples, can only be one variant so final result is straightforward *)
  (binds_to_anf ~exported new_val_binds code), (collect_jump_summary jumps)

(* Can't switch on floats, so have to generate nested if-then-else *)
and apply_float_rule v {values=vs; matrix; total; handlers; ctx; exported} =
  let specialise_float_matrix f =
    let rows = List.filter (function (* All constants must be floats due to type-checking *)
      | ({pat_desc=Tpat_constant c}::_,_) -> f = c
      | _ -> failwith "Wrong rule applied") matrix in
    let new_matrix = List.map (function
     | (_::ps,act) -> (ps, act)
     | _ -> failwith "Wrong rule applied") rows in
    let code, jumps =
     compile_matrix {values=vs; matrix=new_matrix; total; exported;
          handlers=specialise_handlers (Constant f) handlers; ctx=specialise_ctx (Constant f) ctx} in
    (f, code), jumps in

  let get_float = function
        | ({pat_desc=Tpat_constant c}::_,_) -> c
        | _ -> failwith "Can't apply float rule" in
  let floats_used = List.fold_left (fun floats row -> let f = get_float row in
    if List.mem f floats then floats else f::floats) [] matrix in

  (* (constant  * linast * jump_summary) list
    - 'tag' is not an int so can't use the specialise_constructor_matrix function *)
  let specialised_calls = List.map specialise_float_matrix floats_used in
  let cases, jump_summaries = List.split specialised_calls in
  let summary = List.fold_right
    (fun jumps result -> union_jump_summary result (collect_jump_summary jumps))
    jump_summaries [] in
  let _, fail_idx = List.hd handlers in
  (* Made awkward by wanting to return a compound *)
  (List.fold_right (fun (f, body) rest ->
    let test_result = Compound.binary Eq (Imm.const f) v in
    let testid = Ident.create_local "isequal" in
    binds_to_anf [BLet(testid, test_result);]
    (LinastExpr.compound (Compound.mkif (Imm.id testid) body rest)))
   cases (LinastExpr.compound (Compound.fail fail_idx)),
   union_jump_summary summary [(fail_idx, ctx)])
