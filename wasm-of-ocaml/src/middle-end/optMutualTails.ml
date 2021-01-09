(*
   First mark which functions are tail recursive and the tail calls within them.
   TODO: Add check for if tail recursion optimisation actually useful e.g.
     let rec f x = g x
     and g x = 5
     g x looks like a tail call so f will be rewritten, but g won't so no reason to optimise f
 *)
open Linast
open LinastUtils
open Asttypes

type rewrite = {new_fun : Ident.t; new_args : Ident.t}

let mark_tail_call annotations = annotations := TailCall :: (!annotations)
let mark_tail_recursive annotations = annotations := TailRecursive :: (!annotations)
let is_tail_call_optimised annotations = List.mem TailCallOptimised (!annotations)

let mark_single_tail_recursive annotations = annotations := SingleTailRecursive :: (!annotations)

(* Next/Continue/Result variables are shared across all tail recursions,
   only actually include them in program if some function uses them.
   TODO: Slight inefficiency assigning 3 separate variables, each with headers, when only used in specific places.
     Would be slightly better to store as a 3 cell block, best would be to just allocate 3 cells with no overhead. *)
let tailcall_used = ref false
let tailcall_included = ref false (* in case more tail calls found on a later pass *)
let result_id = Ident.create_local "result"
let continue_id = Ident.create_local "continue"
let next_id = Ident.create_local "next"

(*
  Analysis slightly complex to allow analysing functions declared within other functions.
  Keep an ident option for the current tail call being checked for.
   (for mutually recursive functions, this will be a list of all those functions currently tail-callable)
  Unclear if several idents could be optimised simultaneously. (Grain appears to?) e.g.
  let f x =
    let g x = ... ; f (x) in
  ...
  f(x)

  Must track whether current function we are in can have tail calls optimised or not.
    i.e. want to remember that we are within f so can optimise its tail call, but not if g isn't also optimised.
    Keep a list or calls to rewrite, and only perform rewrite if in_tail_optimised_fun true

  Have to be careful when rewriting. If g is itself tail call optimised, then can rewrite the call to f.
  However, if g is just a regular function, then f cannot be rewritten (but a tail called fun within g still could)

  Can only tail call optimise if function is exactly fully applied. Not too many/too few arguments.
  In this case, effectively gets rewritten as a tupled function.
*)

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
TODO: args can be shared globally like next/continue/result, just set its size to the most args ever needed.
  Allocates a global that may not always be needed, but avoids reallocating the same thing in many places.

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
  funcs is a list of the functions we are looking for (and their arities) tail calls to,
    (list of all in-scope possibly tail callable functions. Allows tail calling both ways for nested funs?)
  tbl is the table to update with tail calls *)
(* TODO: Check only ever increasing funcs is valid. (And that it can be extended when checking 'rest' of letrec
     Two (in scope) tail callable functions can always call each other, so may as well mark as tail calls. *)
(* TODO: Use of separate annotations makes this incompatible with the other 2 analysis passes *)
(* TODO: When updating, MUST use TailCallOptimised to determine if allowed to replace calls or not (for multiple)
     separate process needed for updating single functions (see original tailcall code) *)
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
        mark_tail_recursive compound.annotations) binds;
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
    (match List.find_opt (fun (id, _) -> Ident.same called id) funcs with
     (* If we later decide both 'f' and 'called' can be tail call optimised, this call will be rewritten *)
     | Some (_, n) when List.length args = n -> mark_tail_call compound.annotations;
       (* Functions only made mutually recursive if recursive with a function defined in same letrec.
          Therefore f must not be None (in a tail call position for function f)
          and 'called' must be one of the other functions in the tbl.
          Could maybe still optimise, but very rare case. More often, only avoiding 1 stack frame *)
        (match f with Some id when tbl_mem called tbl -> update_tbl id called tbl | _ -> tbl)

      | _ -> tbl
    )
  | _ -> tbl


let new_analyse_program linast = ignore (find_tail_calls None [] Ident.empty linast)



(* ------------------------------------------------------------------------------------------- *)

(* is_tail indicates that this position in the code could be a tail call.
   funcs is (ident, arity) list of things that COULD be replaced in current scope *)
let rec compound_tail_callable is_tail funcs (compound : compound_expr) = match compound.desc with
  (* still need to analyse any functions declared within branches *)
  | CWhile (body1, body2) ->
    ignore (linast_tail_callable false funcs body1);
    ignore (linast_tail_callable false funcs body2);
    false
  (* possibly tail recursive functions will be detected by linast_tail_callable,
     so we just want to search the body of this function for other tail callable functions *)
  | CFunction (_, body)
  | CFor (_, _, _, _, body) ->
    ignore (linast_tail_callable false funcs body); false
  (* The important part, can mark as a tail call if in a suitable location *)
  | CSwitch (_, cases, default) ->
    (* written this way to ensure all branches get analysed *)
    let tail1 = List.fold_left
      (fun tail_call (_, body) -> (linast_tail_callable is_tail funcs body) || tail_call) false cases
    and tail2 = match default with None -> false | Some body -> linast_tail_callable is_tail funcs body in
    tail1 || tail2
  | CIf (_, body1, body2) | CMatchTry (_, body1, body2) ->
    let tail1 = linast_tail_callable is_tail funcs body1
    and tail2 = linast_tail_callable is_tail funcs body2 in
    tail1 || tail2
  | CApp ({desc=ImmIdent f}, args) ->
    (match List.find_opt (fun (id, _) -> Ident.same f id) funcs with
      | Some (_, n) when List.length args = n -> mark_tail_call compound.annotations; true
      | _ -> false
    )
  | _ -> false

and linast_tail_callable is_tail funcs linast = match linast.desc with
  (* Skip single recursive functions, handled separately. *)
  | LLetRec ([_, _, body], rest) ->
    ignore (compound_tail_callable false funcs body);
    linast_tail_callable is_tail funcs rest
  | LLetRec (binds, rest) ->
    (* Functions are marked as tail recursive if they make a tail call
       to any other possibly tail recursive function. Can refine this later *)
    let new_funcs = List.fold_left (fun funcs ->
      (function (id, _, ({desc=CFunction(args, _)} : compound_expr)) -> (id, List.length args)::funcs | _ -> funcs))
      funcs binds in
    List.iter (function
      | (id, _, ({desc=CFunction(args, body); annotations} : compound_expr)) ->

        if (not (List.mem TailCallOptimised (!annotations))) &&
         linast_tail_callable true new_funcs body then mark_tail_recursive annotations
      | _ ->  ()) binds;
    linast_tail_callable is_tail funcs rest
  | LCompound c -> compound_tail_callable is_tail funcs c
  (* Analyse any functions declared within the compound expressions *)
  | LLet (_, _, c, body) | LSeq (c, body) ->
    ignore (compound_tail_callable false funcs c);
    linast_tail_callable is_tail funcs body

(* Entry point, start with no function to optimise *)
let analyse_program linast = ignore (linast_tail_callable false [] linast)

(*
  Next step is to actually replace tail recursive functions.
  Need to create 2 functions for each original one.
  If some function is optimised, also need to introduce result/continue/next variables at top level.
  For each optimised function, the arguments are created as blocks one level higher. (Put all in 1 block?)
  (TODO: 2 cell inefficiency. as we know exactly where these are accessed, could store without box wrapper
    and add operations to directly access (box/unbox?))
  let rec f args = .... <tailcall (args')>

  Becomes

  let fargs' = (0, ..., 0)
  let f args =
    next := _f; continue := true;
    fargs' := args (assign contiguously);
    while (!continue){
      continue := false;
      return := (!next)() -- again, slight inefficiency as we can't make a call with no arguments
    }
    !result

  let f' () =
    let args = !fargs' (read contiguously)
    ...
    <continue := true; fargs' := args'; next := tailfun>

  Need to keep a list of mappings:
  (f, {f', fargs'}) (arity not stored since it must be equal to application arity for tail calls)
*)



(* Rewrite tail recursive call. Caller has already checked this is a valid call to optimise *)
let rewrite_tail_call {new_fun; new_args} args =
  let set_args = List.mapi (fun i arg -> BEffect(Compound.setfield (Imm.id new_args) i arg)) args in
  binds_to_anf
    ((BEffect(Compound.assign next_id (Imm.id new_fun)))::set_args)
    (LinastExpr.compound (Compound.assign continue_id (Imm.const (Const_int 1))))

(* Search for optimisable tail recursive calls and replace them *)
(* in_tail_pos indicates that we are in tail of a tail call optimised function so can rewrite tail calls.
   Cannot rewrite tail calls in functions that weren't rewritten, since they expect an actual result.
   rewrites maps idents of functions to the new idents for them and the idents that hold their args.
   TODO: Must optimise functions in bottom up order to actually spot these nested tail calls *)
(* TODO: Probably a better approach to avoid traversing whole tree multiple times?
     Only traversing within rewritten function, so not that bad.
     Could instead do all transformations and rewriting in one downward pass (i.e. rewrite fun here)
     (then also wouldn't need the TailCallOptimised annotation?).
     Currently don't accumulate more rewrites as we go, just do 1 at a time. Check Grain *)
let rec rewrite_body in_tail_pos rewrites linast = match linast.desc with
  | LLetRec (binds, rest) ->
    (* in_tail_pos will be set active again if bind turns out to be a tail call optimised function *)
    let new_binds = List.map
      (fun (id, global, bind) -> (id, global, rewrite_compound false rewrites bind)) binds in
    {linast with desc = LLetRec(new_binds, rewrite_body in_tail_pos rewrites rest)}
  | LLet (id, global, bind, rest) ->
    {linast with desc =
     LLet(id, global, rewrite_compound false rewrites bind, rewrite_body in_tail_pos rewrites rest)}
  | LSeq (c, rest) ->
    {linast with desc = LSeq(rewrite_compound false rewrites c, rewrite_body in_tail_pos rewrites rest)}
  (* Tail call to replace *)
  (* Not every tail call is to a tail-call-optimised function, so must check it actually has a mapping *)
  | LCompound {desc = CApp({desc = ImmIdent f}, args); annotations}
     when List.mem TailCall (!annotations) ->
     (match List.find_opt (fun (id, _) -> Ident.same id f) rewrites with
       | Some (_, rewrite) -> rewrite_tail_call rewrite args
       | _ -> linast (* not a tail call, nothing left to optimise *))
  (* Recurse on any Linasts within the compound term *)
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
    CFunction(args, rewrite_body (is_tail_call_optimised compound.annotations) rewrites body)}
  | _ -> compound

(* Redefine function to be tail recursive *)
(* returns a new list of bindings to replace the original single binding for the function f *)
let rewrite_function f_id global args body =
  (* mark that at least one function has been optimised to use continue/next/result *)
  tailcall_used := true;

  let new_args = Ident.create_local "args" in
  let new_fun = Ident.create_local (Ident.name f_id) in
  (* Code replacing the body of original function f *)
  (* put args into memory when first called *)
  let arg_binds = List.mapi (fun i arg -> BEffect(Compound.setfield (Imm.id new_args) i (Imm.id arg))) args in
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
      [BEffect(Compound.mkwhile (LinastExpr.compound (Compound.imm (Imm.id continue_id))) while_body)])
    (LinastExpr.compound (Compound.imm (Imm.id result_id)))) in

  (* Code for a new function f' that actually does the work *)
  (* get args out of memory when function body executed *)
  let read_args = List.mapi (fun i arg -> BLet(arg, Compound.field (Imm.id new_args) i)) args in
  let body' = body in
  (* the tail-call function ignores its argument *)
  let dummy_arg = Ident.create_local "_" in
  (* This function is marked as being tail call optimised, so other tail calls in it may be optimised after.
     Also ensures that it is not tail call optimised again by a later pass. *)
  let new_function = Compound.mkfun ~annotations:(ref [TailCallOptimised])
    [dummy_arg] (binds_to_anf read_args body') in

  (* Returns the new bindings, and the remapping to include for optimising tail calls in any of the letrec funcs *)
  (* New list of bindings to insert into the tree in place of original binding.
     Sets up a variable to store the args to f (initially all 0s), f, and the new function f' *)
  [(new_args, Local, Compound.makeblock 0 (List.map (fun _ -> unit_value) args));
   (f_id, global, replaced_function);
   (new_fun, Local, new_function)],
  (f_id, {new_fun; new_args})

(* Scan the whole program and rewrite each tail recursive function, replacing recursive calls within it *)
(* Replace in bottom up order for following reason:
   let rec f x =
     let rec g x = ... f x
     ...
   If both optimisable, initially replace g with 2 functions/while loop and mark it as optimised.
   When f also replaced, as f and g are both tail call optimised functions we can rewrite the call to f in g.
   If f was optimised first, we would need to store those mappings for each subsequent function optimised.
   Instead, f x is seen as optimisable when the body of f is rewritten, which can replace within already optimised g.
 *)
let leave_linast linast = match linast.desc with
  (* Simple recursive functions are handled separately *)
  | LLetRec ([b], rest) -> linast
  | LLetRec (binds, rest)
    when List.exists (fun (_, _, (f : compound_expr)) -> List.mem TailRecursive (!(f.annotations))) binds ->
    (* Replace tail recursive functions with pairs of functions, get list of functions for rewriting tail calls *)
    let rev_binds, rewrites =
      List.fold_left (fun (binds, rewrites) -> (function
        | (id, global, ({desc = CFunction(args, body); annotations} : compound_expr))
          when List.mem TailRecursive (!annotations) ->
          let added, rewrite = (rewrite_function id global args body) in  (added @ binds, rewrite::rewrites)
        | bind -> (bind::binds, rewrites))) ([], []) binds in
    (* Rewriting of applications is achieved by rewrite_body *)
    rewrite_body false rewrites {linast with desc=LLetRec(List.rev rev_binds, rest)}
  | _ -> linast

let optimise linast =
  analyse_program linast;
  let result = (LinastMap.create_mapper ~leave_linast ()) linast in
  if (!tailcall_used) && not (!tailcall_included)
  (* Add bindings for next/continue/result to top of program *)
  then (tailcall_included := true;
    binds_to_anf ~mut:[result_id; continue_id; next_id]
      [BLet(result_id, Compound.imm unit_value);
       BLet(continue_id, Compound.imm unit_value);
       BLet(next_id, Compound.imm unit_value)]
      result)
  else result

(* -------------------------------- NEW VERSION ------------------------------------------------ *)
(*
Can reuse both rewrite_function operations.
For single recursive functions, optTailCalls.rewrite_function does all the work, nothing else required.
For mutual recursive, get updated functions and bindings from each one, then continue converting
the whole tree. Can do the whole thing in a top-down traversal.
*)
(* TODO: Remove 'new' from names of everything *)

let rec rewrite_body_new in_tail_pos rewrites linast = match linast.desc with
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
              let added, rewrite = rewrite_function id global args body in
              (added @ binds, rewrite::rewrites)
            | _ -> failwith "LetRec binding wrongly marked as being mutually tail recursive")
        else (bind::binds, rewrites)) binds ([], rewrites) in
    (* recursively replace tail calls where possible *)
    (* in_tail_pos will be set to true when a function with TailCallOptimised is found *)
    let new_binds = List.map
      (fun (id, global, compound) -> (id, global, rewrite_compound_new false rewrites compound))
      replaced_binds in
    {linast with desc=LLetRec(new_binds, rewrite_body_new in_tail_pos rewrites rest)}
  | LLet (id, global, bind, rest) ->
    {linast with desc =
     LLet(id, global, rewrite_compound_new false rewrites bind, rewrite_body_new in_tail_pos rewrites rest)}
  | LSeq (c, rest) ->
    {linast with desc = LSeq(rewrite_compound_new false rewrites c, rewrite_body_new in_tail_pos rewrites rest)}
  (* Tail call to replace *)
  (* Not every tail call is to a tail-call-optimised function,
     must check it actually has a mapping and that we are in a tail recursive function *)
  | LCompound {desc = CApp({desc = ImmIdent f}, args); annotations}
     when in_tail_pos && List.mem TailCall (!annotations) ->
     (match List.find_opt (fun (id, _) -> Ident.same id f) rewrites with
       | Some (_, rewrite) -> rewrite_tail_call rewrite args
       | _ -> linast (* not a tail call, nothing left to optimise *))
  | LCompound c -> {linast with desc = LCompound(rewrite_compound_new in_tail_pos rewrites c)}

(* CApp handled above due to rewriting an application producing a linast *)
and rewrite_compound_new in_tail_pos rewrites (compound : compound_expr) = match compound.desc with
  | CSwitch (imm, cases, default) ->
    {compound with desc =
      CSwitch(imm,
        List.map (fun (i, body) -> (i, rewrite_body_new in_tail_pos rewrites body)) cases,
        Option.map (rewrite_body_new in_tail_pos rewrites) default)}

  | CIf (imm, body1, body2) ->
      {compound with desc =
        CIf(imm,
          rewrite_body_new in_tail_pos rewrites body1,
          rewrite_body_new in_tail_pos rewrites body2)}

  | CMatchTry (imm, body1, body2) ->
      {compound with desc =
        CMatchTry(imm,
          rewrite_body_new in_tail_pos rewrites body1,
          rewrite_body_new in_tail_pos rewrites body2)}
  (* unlike for optimising 1 function on its own, we recurse on all linasts within a term.
     This allows replacing tail calls to a now optimised function in already optimised nested functions *)
  | CWhile (body1, body2) ->
    {compound with desc = CWhile(rewrite_body_new false rewrites body1, rewrite_body_new false rewrites body2)}
  | CFor (id, imm1, imm2, dir, body) -> {compound with desc =
    CFor (id, imm1, imm2, dir, rewrite_body_new false rewrites body)}
  (* If function has been tail call optimised, can enable replacing mapped tail calls in body *)
  | CFunction (args, body) -> {compound with desc =
    CFunction(args, rewrite_body_new (is_tail_call_optimised compound.annotations) rewrites body)}
  | _ -> compound

let optimise_new linast =
  new_analyse_program linast;
  let result = rewrite_body_new false [] linast in
  if (!tailcall_used) && not (!tailcall_included)
  (* Add bindings for next/continue/result to top of program *)
  then (tailcall_included := true;
    binds_to_anf ~mut:[result_id; continue_id; next_id]
      [BLet(result_id, Compound.imm unit_value);
       BLet(continue_id, Compound.imm unit_value);
       BLet(next_id, Compound.imm unit_value)]
      result)
  else result

