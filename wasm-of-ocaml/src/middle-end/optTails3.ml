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

    (* TODO: Do this later
    let optimise_alone, optimise_together = determine_tailcalls Ident.Set.empty tbl' in
    List.iter (fun (id, _, (compound : compound_expr)) ->
      if Ident.Set.mem id optimise_alone then
        mark_single_tail_recursive compound.annotations
      (* Functions tail call optimised as a group may still have tail calls in, don't rewrite again *)
      else if not (List.mem TailCallOptimised (!(compound.annotations))) &&
        Ident.Set.mem id optimise_together then
        mark_tail_recursive compound.annotations) binds; *)
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
       mark_tail_call compound.annotations; update_tbl id called
      | _ -> ()
    )
  | _ -> ()

let optimise_alone = ref Ident.Set.empty
let optimise_together = ref Ident.Set.empty

let print_tbl tbl =
  Ident.iter (fun id row ->
    let sp = ref false in
    Printf.printf "%s : " (Ident.name id);
    Ident.Set.iter (fun id ->
      if !sp then Printf.printf " " else sp := true;
      Printf.printf "%s" (Ident.name id)) row;
    Printf.printf "\n"
  ) tbl;
  Printf.printf "\n"

let print_set s =
  let sp = ref false in
      Ident.Set.iter (fun id ->
        if !sp then Printf.printf " " else sp := true;
        Printf.printf "%s" (Ident.name id)) s;
  Printf.printf "\n"

(* TODO: Write more functionally *)
let new_analyse_program linast =
  possible_tail_recursive_funcs := Ident.empty;
  find_tail_calls None [] linast;
  print_tbl (!possible_tail_recursive_funcs);
  let alone, together = determine_tailcalls Ident.Set.empty (!possible_tail_recursive_funcs) in
  print_set alone;
  print_set together;
  optimise_alone := alone;
  optimise_together := together



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


(* -------------------------------- NEW VERSION ------------------------------------------------ *)
(*
Can reuse both rewrite_function operations.
For single recursive functions, optTailCalls.rewrite_function does all the work, nothing else required.
For mutual recursive, get updated functions and bindings from each one, then continue converting
the whole tree. Can do the whole thing in a top-down traversal.
*)
(* TODO: Remove 'new' from names of everything *)

(* optimise_together and optimise_alone are used to determine what to do with each function *)
(* TODO: Can SingleTailRecursive and TailRecursive annotations be removed now? *)
let rec rewrite_body_new in_tail_pos rewrites linast = match linast.desc with
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

