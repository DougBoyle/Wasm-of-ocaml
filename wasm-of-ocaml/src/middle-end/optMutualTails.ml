(*
3 different versions implemented:
Version 1 (this file):
  Single-recursive and mutually recursive functions handled entirely separately.
  A function is mutually recursive (and optimised as such) if it makes a tail call to any other
  recursive function (possibly a enclosing function). This happens even if that function isn't
  itself tail-recursive (in which case there may be no benefit to this rewriting).

Version 2:
  Handled together. For deciding if a function should be optimised, just the tail calls to functions
  defined in the same list of binds are considered. Calls to functions which have no tail calls themselves
  are ignored, and functions with only tail calls to themself are handled separately. The rest are
  all made mutually recursive (may miss mutual recursion between functions defined at different nesting depths).
  Avoids making useless optimisations as in Version 1.

Version 3:
  As above but the restriction that functions are defined in the same binding list is removed, instead
  working out which to do on their own/mutually across the whole program at once.
  Compared to the second version, rewrites functions more often (as condition for rewriting is weaker).
  Unclear if the overhead of setup vs benefit of more tails calls is better. May only matter for
  functions which get close to stack limit otherwise.

In all cases, a tail call from one mutually optimised function to any other mutually optimised function
gets rewritten, even if in different binding lists.
Version 3 ensures more tail-calls than version 2 (hence lower stack height). Version 1 may make some tail calls
that version 3 doesn't, but these only save a single call (e.g. f calls g but g never calls back into f).
Version 1 also makes some functions mutually recursive where optimising them alone would be better.
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
