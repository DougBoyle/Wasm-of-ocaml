open Linast
open LinastUtils
open Asttypes

let mark_tail_call annotations = annotations := TailCall :: (!annotations)
let mark_tail_recursive annotations = annotations := TailRecursive :: (!annotations)
let is_tail_call_optimised annotations = List.mem TailCallOptimised (!annotations)
let mark_single_tail_recursive annotations = annotations := SingleTailRecursive :: (!annotations)

(* ------ Determining tail recursive functions (see explanation above for new method) ------- *)

(* Mapping of functions to the (maybe tail recursive) functions they tail call *)
let possible_tail_recursive_funcs = ref (Ident.empty : Ident.Set.t Ident.tbl)

(* returns a pair of sets (optimise_alone, optimise_together), with any other idents not being modified *)
let rec determine_tailcalls alone tbl =
  (* try to remove any elements of table which shouldn't be tail optimised together *)
  let single, non_tail_call =
    Ident.fold_all (fun id callset (single, non_tail_call) ->
      if Ident.Set.is_empty callset
      then (single, Ident.Set.add id non_tail_call)
      (* function only makes tail calls to itself *)
      else if Ident.Set.cardinal callset = 1 && Ident.same id (Ident.Set.choose callset)
      then (Ident.Set.add id single, non_tail_call)
      else (single, non_tail_call)) tbl (Ident.Set.empty, Ident.Set.empty) in
  if Ident.Set.is_empty single && Ident.Set.is_empty non_tail_call
  (* have finished identifying which can be tail call optimised *)
  then (alone, Ident.fold_all (fun id _ set -> Ident.Set.add id set) tbl Ident.Set.empty)
  else
    (* Remove the functions to be handled separately *)
    let to_remove = Ident.Set.union single non_tail_call in
    let new_tbl = Ident.fold_all (fun id calls tbl ->
      if Ident.Set.mem id to_remove then tbl
      else Ident.add id (Ident.Set.diff calls to_remove) tbl) tbl Ident.empty in
    determine_tailcalls (Ident.Set.union alone single) new_tbl

(* Given an ident (known to be in the table) and an ident that is called, add it to the corresponding set *)
let update_tbl f tail_called =
  let new_set = Ident.Set.add tail_called (Ident.find_same f (!possible_tail_recursive_funcs)) in
  possible_tail_recursive_funcs := Ident.add f new_set (Ident.remove f (!possible_tail_recursive_funcs))

let rec find_tail_calls f funcs linast = match linast.desc with
  | LLetRec (binds, rest) ->
    let funcs' = List.fold_left (fun fs ->
      (function (id, _, {c_desc=CFunction(args, _)}) -> (id, List.length args)::fs
        | _ -> fs)) [] binds in

    (* As every ident from funcs' is added to the table.
      Something appearing in funcs later means it is in the table and is a possible tail call *)
    List.iter (fun (f, _) ->
      possible_tail_recursive_funcs := Ident.add f Ident.Set.empty (!possible_tail_recursive_funcs))
      funcs';

    List.iter (function (id, _, {c_desc=CFunction(_, body)}) ->
      find_tail_calls (Some id) (funcs' @ funcs) body
      (* Expression must otherwise be a variable, so tbl will not change *)
      | _ -> ()) binds;
    find_tail_calls f (funcs' @ funcs) rest

  | LCompound c -> find_tail_calls_compound f funcs c
  (* Analyse any functions declared within the compound expressions *)
  | LLet (_, _, c, body) | LSeq (c, body) ->
    find_tail_calls_compound None funcs c;
    find_tail_calls f funcs body

and find_tail_calls_compound f funcs compound = match compound.c_desc with
  (* still need to analyse any functions declared within branches *)
  | CWhile (body1, body2) ->
    find_tail_calls None funcs body1;
    find_tail_calls None funcs body2
  (* possibly tail recursive functions will be detected by linast_tail_callable,
     so we just want to search the body of this function for other tail callable functions *)
  | CFunction (_, body)
  | CFor (_, _, _, _, body) ->
    find_tail_calls None funcs body
  | CSwitch (_, cases, default) ->
    List.iter
      (fun (_, body) -> find_tail_calls f funcs body) cases;
    (match default with None -> () | Some body -> find_tail_calls f funcs body)
  | CIf (_, body1, body2) | CMatchTry (_, body1, body2) ->
    find_tail_calls f funcs body1;
    find_tail_calls f funcs body2
  | CApp ({i_desc=ImmIdent called}, args) ->
    (* Check that:
       1) We are in a recursive function and at a tail-call position within it.
       2) The function being called could also be a tail-call optimised function. *)
    (match f, List.find_opt (fun (id, _) -> Ident.same called id) funcs with
     (* If we later decide both 'f' and 'called' can be tail call optimised, this call will be rewritten *)
     | Some id, Some (_, n) when List.length args = n ->
       mark_tail_call compound.c_annotations; update_tbl id called
      | _ -> ()
    )
  | _ -> ()

let optimise_alone = ref Ident.Set.empty
let optimise_together = ref Ident.Set.empty

let analyse_program linast =
  possible_tail_recursive_funcs := Ident.empty;
  find_tail_calls None [] linast;
  let alone, together = determine_tailcalls Ident.Set.empty (!possible_tail_recursive_funcs) in
  optimise_alone := alone;
  optimise_together := together

(*
  ------- Optimising a function on its own: -------
  let rec f args = .... <f (args')>

  Becomes

  let rec f args* =    -- still recursive due to possibly having non tail-recursive calls too.
    continue = true; result = 0; mut_args = args*;
    while continue {
      continue = false;
      result =
        let args = mut_args in  -- allows keeping the assumption that values are immutable everywhere else
        ...
        <mut_args = args'; continue = true>
    }
    result
*)

(* Rewrite tail recursive call *)
let rewrite_tail_call_single continue_id args new_args =
  binds_to_anf
     (List.map (fun (arg, new_arg) -> BEffect(Compound.assign arg new_arg)) (List.combine args new_args))
     (LinastExpr.compound (Compound.assign continue_id (Imm.const (Const_int 1))))

(* Search everywhere a tail recursive call for the given function could occur and replace it *)
let rec rewrite_body_single f_id continue_id args linast = match linast.desc with
  | LLetRec (binds, rest) ->
    {linast with desc = LLetRec(binds, rewrite_body_single f_id continue_id args rest)}
  | LLet (id, global, bind, rest) ->
    {linast with desc = LLet(id, global, bind, rewrite_body_single f_id continue_id args rest)}
  | LSeq (c, rest) ->
    {linast with desc = LSeq(c, rewrite_body_single f_id continue_id args rest)}
  (* Tail call to replace *)
  | LCompound {c_desc = CApp({i_desc = ImmIdent f}, new_args); c_annotations}
     when Ident.same f_id f && List.mem TailCall (!c_annotations) ->
     rewrite_tail_call_single continue_id args new_args
  (* Recurse on any Linasts within the compound term *)
  | LCompound c -> {linast with desc = LCompound(rewrite_compound_single f_id continue_id args c)}

and rewrite_compound_single f_id continue_id args compound = match compound.c_desc with
  | CSwitch (imm, cases, default) ->
    {compound with c_desc =
      CSwitch(imm,
        List.map (fun (i, body) -> (i, rewrite_body_single f_id continue_id args body)) cases,
        Option.map (rewrite_body_single f_id continue_id args) default)}

  | CIf (imm, body1, body2) ->
      {compound with c_desc =
        CIf(imm,
          rewrite_body_single f_id continue_id args body1,
          rewrite_body_single f_id continue_id args body2)}

  | CMatchTry (imm, body1, body2) ->
      {compound with c_desc =
        CMatchTry(imm,
          rewrite_body_single f_id continue_id args body1,
          rewrite_body_single f_id continue_id args body2)}
  | _ -> compound

(* Redefine function to be tail recursive on its own *)
let rewrite_function_single f_id args body =
  (* Create new formal parameters for the function, as the original paramters
     will now be assigned to from mutable copies rather than coming from function argument/closure *)
  let new_params = List.map (fun id -> Ident.create_local (Ident.name id)) args in
  let mut_args = List.map (fun id -> Ident.create_local (Ident.name id)) args in
  let initial_binds = List.map (fun (arg, param) -> BLet(arg, Compound.imm (Imm.id param)))
    (List.combine mut_args new_params) in
  let while_binds = List.map (fun (arg, mut_arg) -> BLet(arg, Compound.imm (Imm.id mut_arg)))
    (List.combine args mut_args) in
  let continue_id = Ident.create_local "continue"
  and result_id = Ident.create_local "result_mut"
  (* Need to bind the result to an immediate before LAssign can be used *)
  and temp_result_id = Ident.create_local "result" in

  let while_body =
    (* binding of arg = mut_arg also marked as mutable, since we don't want to allow use of mutable
       variable to leak into body of function *)
    binds_to_anf ~mut:(args) ((BEffect(Compound.assign continue_id (Imm.const (Const_int 0))))::while_binds)
    (* Need to rewrite the body of the function so that it binds the result to 'result'. i.e. linearise expression  *)
    (* Since CAssign takes an imm, first need to bind the result to a 'temp_reuslt_id' *)
    (OptConstants.rewrite_tree (LinastExpr.mklet temp_result_id Local)
      (rewrite_body_single f_id continue_id mut_args body)
      (LinastExpr.compound (Compound.assign result_id (Imm.id temp_result_id)))) in

  let new_body =
    binds_to_anf ~mut:(continue_id :: result_id :: mut_args)
    ((BLet(continue_id, Compound.imm (Imm.const (Const_int 1)))) ::
     (BLet(result_id, Compound.imm unit_value)) ::
     initial_binds @
     [BEffect(Compound.mkwhile (LinastExpr.compound (Compound.imm (Imm.id continue_id))) while_body)])
    (LinastExpr.compound (Compound.imm (Imm.id result_id))) in
  (* Returns a new function, so TailRecursive annotation and any others are removed *)
  Compound.mkfun new_params new_body

(*
  ------- Optimising a mutually recursive function: -------
  args*, next, continue and result are shared globally.
  let rec f args = .... <g (args')>     (where g is a function f is mutually recursive with, or f itself)

  Becomes

  let rec f args =
    next = _f; continue = true; args* = args;
    while (continue){
      continue = false;
      result = (next)()
    }
    result

  and _f () =
    let args = args*
    ...
    <continue = true; args* := args'; next := g>
*)

let tailcall_used = ref false
let tailcall_included = ref false (* in case more tail calls found on a later pass *)
let max_args = ref 0
let args_id = Ident.create_local "args"
let result_id = Ident.create_local "result"
let continue_id = Ident.create_local "continue"
let next_id = Ident.create_local "next"

(* Rewrite tail recursive call. Caller has already checked this is a valid call to optimise *)
let rewrite_tail_call new_fun args =
  let set_args = List.mapi (fun i arg -> BEffect(Compound.setfield (Imm.id args_id) i arg)) args in
  binds_to_anf
    ((BEffect(Compound.assign next_id (Imm.id new_fun)))::set_args)
    (LinastExpr.compound (Compound.assign continue_id (Imm.const (Const_int 1))))

(* Redefine function to be tail recursive *)
(* returns a new list of bindings to replace the original single binding for the function f *)
let rewrite_function f_id global args body =
  (* mark that at least one function has been optimised to use continue/next/result *)
  tailcall_used := true;
  max_args := max (List.length args) (!max_args);

  let new_fun = Ident.create_local (Ident.name f_id) in
  (* Code replacing the body of original function f *)
  (* put args into memory when first called *)
  let arg_binds = List.mapi (fun i arg -> BEffect(Compound.setfield (Imm.id args_id) i (Imm.id arg))) args in
  let setup =
    (BEffect(Compound.assign continue_id (Imm.const (Const_int 1))))::
    (BEffect(Compound.assign next_id (Imm.id new_fun)))::arg_binds in
  (* Since CAssign takes an imm, first need to bind the result to a 'temp_result_id' *)
  let temp_result_id = Ident.create_local "result" in
  let while_body = binds_to_anf
    [BEffect(Compound.assign continue_id unit_value);
     BLet(temp_result_id, Compound.app (Imm.id next_id) [unit_value]);]
    (LinastExpr.compound (Compound.assign result_id (Imm.id temp_result_id))) in
  (* The rewritten function is never tail recursive, so no need to mark with TailCallOptimised *)
  let replaced_function = Compound.mkfun args
    (binds_to_anf (setup @
      [BEffect(Compound.mkwhile
        (LinastExpr.compound (Compound.imm (Imm.id continue_id))) while_body)])
    (LinastExpr.compound (Compound.imm (Imm.id result_id)))) in

  (* Code for a new function f' that actually does the work *)
  (* get args out of memory when function body executed *)
  let read_args = List.mapi (fun i arg -> BLet(arg, Compound.field (Imm.id args_id) i)) args in
  let body' = body in
  (* the tail-call function ignores its argument *)
  let dummy_arg = Ident.create_local "_" in
  (* This function is marked as being tail call optimised, so other tail calls in it may be optimised after.
     Also ensures that it is not tail call optimised again by a later pass. *)
  let new_function = Compound.mkfun ~annotations:(ref [TailCallOptimised])
    [dummy_arg] (binds_to_anf read_args body') in

  (* Returns the new bindings, and the remapping to include for optimising tail calls in any of the letrec funcs *)
  (* New list of bindings to insert into the tree in place of original binding.
     Sets up f, and the new function f' *)
  [(f_id, global, replaced_function);
   (new_fun, Local, new_function)],
  (f_id, new_fun)

(* optimise_together and optimise_alone are used to determine what to do with each function *)
let rec rewrite_body in_tail_pos rewrites linast = match linast.desc with
  | LLetRec (binds, rest) ->
    (* Rewrite functions based on if they are single or mutually recursive *)
    let replaced_binds, rewrites =
      List.fold_right (fun ((id, global, compound) as bind) (binds, rewrites) ->
        if Ident.Set.mem id (!optimise_alone)
        then (match compound.c_desc with
           | CFunction(args, body) ->
             let new_f = rewrite_function_single id args body in
             ((id, global, new_f)::binds, rewrites)
           | _ -> failwith "LetRec binding wrongly marked as being single tail recursive")
        else if Ident.Set.mem id (!optimise_together)
        then (match compound.c_desc with
            | CFunction(args, body) ->
              let added, rewrite = rewrite_function id global args body in
              (added @ binds, rewrite::rewrites)
            | _ -> failwith "LetRec binding wrongly marked as being mutually tail recursive")
        else (bind::binds, rewrites)) binds ([], rewrites) in
    (* recursively replace tail calls where possible *)
    (* in_tail_pos will be set to true when a function with TailCallOptimised is found *)
    let new_binds = List.map
      (fun (id, global, compound) -> (id, global, rewrite_compound false rewrites compound))
      replaced_binds in
    {linast with desc=LLetRec(new_binds, rewrite_body in_tail_pos rewrites rest)}
  | LLet (id, global, bind, rest) ->
    {linast with desc =
     LLet(id, global, rewrite_compound false rewrites bind, rewrite_body in_tail_pos rewrites rest)}
  | LSeq (c, rest) ->
    {linast with desc = LSeq(rewrite_compound false rewrites c, rewrite_body in_tail_pos rewrites rest)}
  (* Tail call to replace *)
  (* Not every tail call is to a tail-call-optimised function,
     must check it actually has a mapping and that we are in a tail recursive function *)
  | LCompound {c_desc = CApp({i_desc = ImmIdent f}, args); c_annotations}
     when in_tail_pos && List.mem TailCall (!c_annotations) ->
     (match List.find_opt (fun (id, _) -> Ident.same id f) rewrites with
       | Some (_, rewrite) -> rewrite_tail_call rewrite args
       | _ -> linast)
  | LCompound c -> {linast with desc = LCompound(rewrite_compound in_tail_pos rewrites c)}

(* CApp handled above due to rewriting an application producing a linast *)
and rewrite_compound in_tail_pos rewrites compound = match compound.c_desc with
  | CSwitch (imm, cases, default) ->
    {compound with c_desc =
      CSwitch(imm,
        List.map (fun (i, body) -> (i, rewrite_body in_tail_pos rewrites body)) cases,
        Option.map (rewrite_body in_tail_pos rewrites) default)}

  | CIf (imm, body1, body2) ->
      {compound with c_desc =
        CIf(imm,
          rewrite_body in_tail_pos rewrites body1,
          rewrite_body in_tail_pos rewrites body2)}

  | CMatchTry (imm, body1, body2) ->
      {compound with c_desc =
        CMatchTry(imm,
          rewrite_body in_tail_pos rewrites body1,
          rewrite_body in_tail_pos rewrites body2)}
  | CWhile (body1, body2) ->
    {compound with c_desc = CWhile(rewrite_body false rewrites body1, rewrite_body false rewrites body2)}
  | CFor (id, imm1, imm2, dir, body) -> {compound with c_desc =
    CFor (id, imm1, imm2, dir, rewrite_body false rewrites body)}
  (* If function has been tail call optimised, can enable replacing mapped tail calls in body *)
  | CFunction (args, body) -> {compound with c_desc =
    CFunction(args, rewrite_body (is_tail_call_optimised compound.c_annotations) rewrites body)}
  | _ -> compound

let adjust_max_args linast = match linast.desc with
  | LLet(id, local, block, rest) when Ident.same id args_id ->
    {linast with desc=LLet(id, local, Compound.makeblock 0 (List.init (!max_args) (fun _ -> unit_value)), rest)}
  | _ -> failwith "Program should start with binding for block of mutually recursive arguments"

let optimise linast =
  let original_max = !max_args in
  analyse_program linast;
  let result = rewrite_body false [] linast in
  if (!tailcall_used) && not (!tailcall_included)
  (* Add bindings for next/continue/result to top of program *)
  then (tailcall_included := true;
    binds_to_anf ~mut:[result_id; continue_id; next_id]
      [BLet(args_id, Compound.makeblock 0 (List.init (!max_args) (fun _ -> unit_value)));
       BLet(result_id, Compound.imm unit_value);
       BLet(continue_id, Compound.imm unit_value);
       BLet(next_id, Compound.imm unit_value)]
      result)
  (* Number of cells needed for tail call arguments has increase *)
  else if original_max < (!max_args)
  then adjust_max_args result
  else result
