open Linast
open LinastUtils
open Asttypes

type rewrite = {new_fun : Ident.t; new_args : Ident.t}

let mark_single_tail_recursive annotations = annotations := SingleTailRecursive :: (!annotations)

(* ------ New approach to determining tail-call optimisations to make ------ *)
(*
Issue with more naive approach is that, if f and g are defined together, then a tail call to g in f is used
to justify converting f to two separate functions. However, g may not be tail-call optimisable in which
case f still makes a regular function call to it and the optimisation achieves nothing.
Its also not the case that the programmer should have just not defined them together, as they could be
mutually recursive just with none of g's calls to f (or itself) being in a tail-call position.

Instead, when a letrec binding is reached, a table is built mapping each ident to a Set of the
tail calls to any of the idents being bound currently (not tail calls to functions defined elsewhere).
After the body of each binding is processed we do the following:
- If any function has no tail calls, it is removed as are any tail calls to it.
- If a function has only tail calls to itself, it is optimised individually and tails calls to it are also removed.
- This is repeated until there are no changes.
At this point, all the remaining functions really do benefit from tail call optimisation so are optimised together.

This still ignores the possibility of a function being mutually tail-recursive with a function defined
within it, but this then requires analysing the whole program all at once (rather than one letrec bind at a time)
and is rarely expected to occur.
The closure for the inner function would also still be constructed each time the outer function is called.
If both levels of functions are tail callable on their own, the inner one will make tail calls back to the outer one,
but not the reverse due to the scope within which tail calls are optimised.

This approach also makes better choices about when to optimise a function on its own (in which case no new function
is create) or as part of a set of functions each split into two.
Rather than requiring a function to be declared on its own, we do the analysis of the group and, if a function
only makes tail calls to itself, it is removed and optimised on its own (and calls to that function ignored).
Other functions may still call this one, but at most 1 extra stack frame will be allocated for it as it then
never makes a tail call back to one of the other functions.
*)

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
let update_tbl f tail_called tbl =
  let new_set = Ident.Set.add tail_called (Ident.find_same f tbl) in
  Ident.add f new_set (Ident.remove f tbl)

let tbl_mem called tbl =
  match Ident.find_same called tbl with
    | _ -> true
    | exception Not_found -> false

(* f (optional) is the current function we are in,
  funcs is a list of the possibly-tail callable functions (and their arities) that are in scope,
  tbl is the table to update with tail calls to a function defined in the same block as f *)
let rec find_tail_calls f funcs tbl linast = match linast.desc with
  | LLetRec (binds, rest) ->
    let funcs' = List.fold_left (fun fs ->
      (function (id, _, ({desc=CFunction(args, _)} : compound_expr)) -> (id, List.length args)::fs
        | _ -> fs)) [] binds in
    let initial_tbl = List.fold_right
      (fun (f, _) tbl -> Ident.add f Ident.Set.empty tbl) funcs' Ident.empty in
    let tbl' = List.fold_left (fun tbl -> (
      function (id, _, ({desc=CFunction(_, body)} : compound_expr)) ->
      find_tail_calls (Some id) (funcs' @ funcs) tbl body
      (* Expression must otherwise be a variable, so tbl will not change *)
      | _ -> tbl)) initial_tbl binds in
    let optimise_alone, optimise_together = determine_tailcalls Ident.Set.empty tbl' in
    List.iter (fun (id, _, (compound : compound_expr)) ->
      if Ident.Set.mem id optimise_alone then
        mark_single_tail_recursive compound.annotations
      (* Functions tail call optimised as a group may still have tail calls in, don't rewrite again *)
      else if not (List.mem TailCallOptimised (!(compound.annotations))) &&
        Ident.Set.mem id optimise_together then
        OptMutualTails.mark_tail_recursive compound.annotations) binds;
    find_tail_calls f (funcs' @ funcs) tbl rest
  | LCompound c -> find_tail_calls_compound f funcs tbl c
  (* Analyse any functions declared within the compound expressions *)
  | LLet (_, _, c, body) | LSeq (c, body) ->
    ignore (find_tail_calls_compound None funcs Ident.empty c);
    find_tail_calls f funcs tbl body

and find_tail_calls_compound f funcs tbl (compound : compound_expr) = match compound.desc with
  (* still need to analyse any functions declared within branches *)
  | CWhile (body1, body2) ->
    ignore (find_tail_calls None funcs Ident.empty body1);
    ignore (find_tail_calls None funcs Ident.empty body2);
    tbl
  (* possibly tail recursive functions will be detected by linast_tail_callable,
     so we just want to search the body of this function for other tail callable functions *)
  | CFunction (_, body)
  | CFor (_, _, _, _, body) ->
    ignore (find_tail_calls None funcs Ident.empty body); tbl
  (* The important part, can mark as a tail call if in a suitable location *)
  | CSwitch (_, cases, default) ->
    (* written this way to ensure all branches get analysed *)
    let tbl = List.fold_left
      (fun tbl (_, body) -> find_tail_calls f funcs tbl body) tbl cases in
    (match default with None -> tbl | Some body -> find_tail_calls f funcs tbl body)
  | CIf (_, body1, body2) | CMatchTry (_, body1, body2) ->
    let tbl = find_tail_calls f funcs tbl body1 in
    find_tail_calls f funcs tbl body2
  | CApp ({desc=ImmIdent called}, args) ->
    (match f, List.find_opt (fun (id, _) -> Ident.same called id) funcs with
     (* If we later decide both 'f' and 'called' can be tail call optimised, this call will be rewritten *)
     | Some id, Some (_, n) when List.length args = n -> OptMutualTails.mark_tail_call compound.annotations;
       (* Functions only made mutually recursive if recursive with a function defined in same letrec.
          Therefore 'called' must be one of the other functions in the tbl.  *)
        if tbl_mem called tbl then update_tbl id called tbl else tbl
      | _ -> tbl)
  | _ -> tbl

let analyse_program linast = ignore (find_tail_calls None [] Ident.empty linast)

let rec rewrite_body in_tail_pos rewrites linast = match linast.desc with
  | LLetRec (binds, rest) ->
    (* Rewrite functions based on if they are single or mutually recursive *)
    let replaced_binds, rewrites =
      List.fold_right (fun ((id, global, (compound : compound_expr)) as bind) (binds, rewrites) ->
        if List.mem SingleTailRecursive (!(compound.annotations))
        then (match compound.desc with
           | CFunction(args, body) ->
             let new_f = OptTailCalls.rewrite_function id args body in
             ((id, global, new_f)::binds, rewrites)
           | _ -> failwith "LetRec binding wrongly marked as being single tail recursive")
        else if List.mem TailRecursive (!(compound.annotations))
        then (match compound.desc with
            | CFunction(args, body) ->
              let added, rewrite = OptMutualTails.rewrite_function id global args body in
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
       | Some (_, rewrite) -> OptMutualTails.rewrite_tail_call rewrite args
       | _ -> linast (* not a tail call, nothing left to optimise *))
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
  (* unlike for optimising 1 function on its own, we recurse on all linasts within a term.
     This allows replacing tail calls to a now optimised function in already optimised nested functions *)
  | CWhile (body1, body2) ->
    {compound with desc = CWhile(rewrite_body false rewrites body1, rewrite_body false rewrites body2)}
  | CFor (id, imm1, imm2, dir, body) -> {compound with desc =
    CFor (id, imm1, imm2, dir, rewrite_body false rewrites body)}
  (* If function has been tail call optimised, can enable replacing mapped tail calls in body *)
  | CFunction (args, body) -> {compound with desc =
    CFunction(args, rewrite_body (OptMutualTails.is_tail_call_optimised compound.annotations) rewrites body)}
  | _ -> compound

let optimise linast =
  analyse_program linast;
  let result = rewrite_body false [] linast in
  if (!OptMutualTails.tailcall_used) && not (!OptMutualTails.tailcall_included)
  (* Add bindings for next/continue/result to top of program *)
  then (OptMutualTails.tailcall_included := true;
    binds_to_anf ~mut:[OptMutualTails.result_id; OptMutualTails.continue_id; OptMutualTails.next_id]
      [BLet(OptMutualTails.result_id, Compound.imm unit_value);
       BLet(OptMutualTails.continue_id, Compound.imm unit_value);
       BLet(OptMutualTails.next_id, Compound.imm unit_value)]
      result)
  else result

