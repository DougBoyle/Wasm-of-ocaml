open Linast
open LinastUtils
open Asttypes

let mark_single_tail_recursive annotations = annotations := SingleTailRecursive :: (!annotations)

(* Use one global to store the arguments to any mutually tail recursive function *)
let max_args = ref 0
let args_id = Ident.create_local "args"

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
      (function (id, _, ({desc=CFunction(args, _)} : compound_expr)) -> (id, List.length args)::fs
        | _ -> fs)) [] binds in

    (* As every ident from funcs' is added to the table.
      Something appearing in funcs later means it is in the table and is a possible tail call *)
    List.iter (fun (f, _) ->
      possible_tail_recursive_funcs := Ident.add f Ident.Set.empty (!possible_tail_recursive_funcs))
      funcs';

    List.iter (function (id, _, ({desc=CFunction(_, body)} : compound_expr)) ->
      find_tail_calls (Some id) (funcs' @ funcs) body
      (* Expression must otherwise be a variable, so tbl will not change *)
      | _ -> ()) binds;
    find_tail_calls f (funcs' @ funcs) rest

  | LCompound c -> find_tail_calls_compound f funcs c
  (* Analyse any functions declared within the compound expressions *)
  | LLet (_, _, c, body) | LSeq (c, body) ->
    find_tail_calls_compound None funcs c;
    find_tail_calls f funcs body

and find_tail_calls_compound f funcs (compound : compound_expr) = match compound.desc with
  (* still need to analyse any functions declared within branches *)
  | CWhile (body1, body2) ->
    find_tail_calls None funcs body1;
    find_tail_calls None funcs body2
  (* possibly tail recursive functions will be detected by linast_tail_callable,
     so we just want to search the body of this function for other tail callable functions *)
  | CFunction (_, body)
  | CFor (_, _, _, _, body) ->
    find_tail_calls None funcs body
  (* The important part, can mark as a tail call if in a suitable location *)
  | CSwitch (_, cases, default) ->
    (* written this way to ensure all branches get analysed *)
    List.iter
      (fun (_, body) -> find_tail_calls f funcs body) cases;
    (match default with None -> () | Some body -> find_tail_calls f funcs body)
  | CIf (_, body1, body2) | CMatchTry (_, body1, body2) ->
    find_tail_calls f funcs body1;
    find_tail_calls f funcs body2
  | CApp ({desc=ImmIdent called}, args) ->
    (* Check that:
       1) We are in a recursive function and at a tail-call position within it.
       2) The function being called could also be a tail-call optimised function. *)
    (match f, List.find_opt (fun (id, _) -> Ident.same called id) funcs with
     (* If we later decide both 'f' and 'called' can be tail call optimised, this call will be rewritten *)
     | Some id, Some (_, n) when List.length args = n ->
       OptMutualTails.mark_tail_call compound.annotations; update_tbl id called
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

(* Rewrite tail recursive call. Caller has already checked this is a valid call to optimise *)
let rewrite_tail_call new_fun args =
  let set_args = List.mapi (fun i arg -> BEffect(Compound.setfield (Imm.id args_id) i arg)) args in
  binds_to_anf
    ((BEffect(Compound.assign OptMutualTails.next_id (Imm.id new_fun)))::set_args)
    (LinastExpr.compound (Compound.assign OptMutualTails.continue_id (Imm.const (Const_int 1))))

(* Redefine function to be tail recursive *)
(* returns a new list of bindings to replace the original single binding for the function f *)
let rewrite_function f_id global args body =
  (* mark that at least one function has been optimised to use continue/next/result *)
  OptMutualTails.tailcall_used := true;
  max_args := max (List.length args) (!max_args);

  let new_fun = Ident.create_local (Ident.name f_id) in
  (* Code replacing the body of original function f *)
  (* put args into memory when first called *)
  let arg_binds = List.mapi (fun i arg -> BEffect(Compound.setfield (Imm.id args_id) i (Imm.id arg))) args in
  let setup =
    (BEffect(Compound.assign OptMutualTails.continue_id (Imm.const (Const_int 1))))::
    (BEffect(Compound.assign OptMutualTails.next_id (Imm.id new_fun)))::arg_binds in
  (* Since CAssign takes an imm, first need to bind the result to a 'temp_result_id' *)
  let temp_result_id = Ident.create_local "result" in
  let while_body = binds_to_anf
    [BEffect(Compound.assign OptMutualTails.continue_id unit_value);
     BLet(temp_result_id, Compound.app (Imm.id OptMutualTails.next_id) [unit_value]);]
    (LinastExpr.compound (Compound.assign OptMutualTails.result_id (Imm.id temp_result_id))) in
  (* The rewritten function is never tail recursive, so no need to mark with TailCallOptimised *)
  let replaced_function = Compound.mkfun args
    (binds_to_anf (setup @
      [BEffect(Compound.mkwhile
        (LinastExpr.compound (Compound.imm (Imm.id OptMutualTails.continue_id))) while_body)])
    (LinastExpr.compound (Compound.imm (Imm.id OptMutualTails.result_id)))) in

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
      List.fold_right (fun ((id, global, (compound : compound_expr)) as bind) (binds, rewrites) ->
        if Ident.Set.mem id (!optimise_alone)
        then (match compound.desc with
           | CFunction(args, body) ->
             let new_f = OptTailCalls.rewrite_function id args body in
             ((id, global, new_f)::binds, rewrites)
           | _ -> failwith "LetRec binding wrongly marked as being single tail recursive")
        else if Ident.Set.mem id (!optimise_together)
        then (match compound.desc with
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
  | LCompound {desc = CApp({desc = ImmIdent f}, args); annotations}
     when in_tail_pos && List.mem TailCall (!annotations) ->
     (match List.find_opt (fun (id, _) -> Ident.same id f) rewrites with
       | Some (_, rewrite) -> rewrite_tail_call rewrite args
       | _ -> linast)
  | LCompound c -> {linast with desc = LCompound(rewrite_compound in_tail_pos rewrites c)}

(* CApp handled above due to rewriting an application producing a linast *)
and rewrite_compound in_tail_pos rewrites (compound : compound_expr) = match compound.desc with
  | CSwitch (imm, cases, default) ->
    {compound with desc =
      CSwitch(imm,
        List.map (fun (i, body) -> (i, rewrite_body in_tail_pos rewrites body)) cases,
        Option.map (rewrite_body in_tail_pos rewrites) default)}

  | CIf (imm, body1, body2) ->
      {compound with desc =
        CIf(imm,
          rewrite_body in_tail_pos rewrites body1,
          rewrite_body in_tail_pos rewrites body2)}

  | CMatchTry (imm, body1, body2) ->
      {compound with desc =
        CMatchTry(imm,
          rewrite_body in_tail_pos rewrites body1,
          rewrite_body in_tail_pos rewrites body2)}
  | CWhile (body1, body2) ->
    {compound with desc = CWhile(rewrite_body false rewrites body1, rewrite_body false rewrites body2)}
  | CFor (id, imm1, imm2, dir, body) -> {compound with desc =
    CFor (id, imm1, imm2, dir, rewrite_body false rewrites body)}
  (* If function has been tail call optimised, can enable replacing mapped tail calls in body *)
  | CFunction (args, body) -> {compound with desc =
    CFunction(args, rewrite_body (OptMutualTails.is_tail_call_optimised compound.annotations) rewrites body)}
  | _ -> compound

let adjust_max_args linast = match linast.desc with
  | LLet(id, local, block, rest) when Ident.same id args_id ->
    {linast with desc=LLet(id, local, Compound.makeblock 0 (List.init (!max_args) (fun _ -> unit_value)), rest)}
  | _ -> failwith "Program should start with binding for block of mutually recursive arguments"

let optimise linast =
  let original_max = !max_args in
  analyse_program linast;
  let result = rewrite_body false [] linast in
  if (!OptMutualTails.tailcall_used) && not (!OptMutualTails.tailcall_included)
  (* Add bindings for next/continue/result to top of program *)
  then (OptMutualTails.tailcall_included := true;
    binds_to_anf ~mut:[OptMutualTails.result_id; OptMutualTails.continue_id; OptMutualTails.next_id]
      [BLet(args_id, Compound.makeblock 0 (List.init (!max_args) (fun _ -> unit_value)));
       BLet(OptMutualTails.result_id, Compound.imm unit_value);
       BLet(OptMutualTails.continue_id, Compound.imm unit_value);
       BLet(OptMutualTails.next_id, Compound.imm unit_value)]
      result)
  (* Number of cells needed for tail call arguments has increase *)
  else if original_max < (!max_args)
  then adjust_max_args result
  else result

