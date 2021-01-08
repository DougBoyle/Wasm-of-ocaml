(*
   First mark which functions are tail recursive and the tail calls within them.
   TODO: For now only handle recursive functions, not mutually recursive functions.
 *)
open Linast
open LinastUtils
open Asttypes

let mark_tail_call annotations = annotations := TailCall :: (!annotations)
let mark_tail_recursive annotations = annotations := TailRecursive :: (!annotations)

(*
  Analysis slightly complex to allow analysing functions declared within other functions.
  Keep an ident option for the current tail call being checked for.
   (for mutually recursive functions, this will be a list of all those functions currently tail-callable)
  Unclear if several idents could be optimised simultaneously. (Grain appears to?) e.g.
  let f x =
    let g x = ... ; f (x) in
  ...
  f(x)
  Seems like it shouldn't be, as the inner f(x) will not be the last thing to happen.
  Leave inlining to work out when g(x) is in a tail call position so both f(x)'s can be tail calls.

  TODO: As long as references copied to locals in the correct places, can TCO a function which
        has both tail calls and non-tail calls, as shared values can be overwritten at that point.
        Means that a recursive binding is kept, as it may be used recursively still

  Can only tail call optimise if function is exactly fully applied. Not too many/too few arguments.
  In this case, effectively gets rewritten as a tupled function.
*)

let rec compound_tail_callable func (compound : compound_expr) = match compound.desc with
  (* still need to analyse any functions declared within branches *)
  | CWhile (body1, body2) ->
    ignore (linast_tail_callable None body1);
    ignore (linast_tail_callable None body2);
    false
  (* possibly tail recursive functions will be detected by linast_tail_callable,
     so we just want to search the body of this function for other tail callable functions *)
  | CFunction (_, body)
  | CFor (_, _, _, _, body) ->
    ignore (linast_tail_callable None body); false
  (* The important part, can mark as a tail call if in a suitable location *)
  | CSwitch (_, cases, default) ->
    (* written this way to ensure all branches get analysed *)
    let tail1 = List.fold_left
      (fun tail_call (_, body) -> (linast_tail_callable func body) || tail_call) false cases
    and tail2 = match default with None -> false | Some body -> linast_tail_callable func body in
    tail1 || tail2
  | CIf (_, body1, body2) | CMatchTry (_, body1, body2) ->
    let tail1 = linast_tail_callable func body1
    and tail2 = linast_tail_callable func body2 in
    tail1 || tail2
  | CApp ({desc=ImmIdent f}, args) -> (match func with
    | Some (id, n) when id = f && List.length args = n -> mark_tail_call compound.annotations; true
    | _ -> false)
  | _ -> false

and linast_tail_callable func linast = match linast.desc with
  (* TODO: Allow optimising mutually recursive functions too *)
  (* For now, only perform tail call analysis of single recursive functions *)
  | LLetRec ([id, _, f], rest) ->
    (match f.desc with
      | CFunction(args, body) ->
        (* Now looking for a different function  than 'func' *)
        if linast_tail_callable (Some (id, List.length args)) body then mark_tail_recursive f.annotations
      (* some ident for a function that is already defined *)
      | _ ->  ());
    linast_tail_callable func rest
  (* For now, if mutually recursive functions defined, just ignore analysis of each binding *)
  | LLetRec (binds, rest) ->
    List.iter (fun (_, _, c) -> ignore (compound_tail_callable None c)) binds;
    linast_tail_callable func rest
  | LCompound c -> compound_tail_callable func c
  (* Analyse any functions declared within the compound expressions *)
  | LLet (_, _, c, body) | LSeq (c, body) ->
    ignore (compound_tail_callable None c);
    linast_tail_callable func body

(* Entry point, start with no function to optimise *)
let analyse_program linast = ignore (linast_tail_callable None linast)

(*
  Next step is to actually replace tail recursive functions.
  When done for mutual recursive functions, need to create 2 functions for each original one.
  Simpler for single recursive functions:
  let rec f args = .... <tailcall (args')>

  Becomes

  let rec f args* =    -- still recursive due to non tail-recursive calls potentially.
    continue = true;
    result = 0;
    _args = args*
    while continue {
      continue = false;
      args = _args;
      result =
        ...
        <continue = true; _args = args'; 0>;
      0  -- assign instruction gets compiled to a store, doesn't return a value, so need value for while loop
    }
    result

  We now require an 'Assign' operation for mutable locals, to avoid having indirection to memory to set _args.
  Since some of the original arguments may come from a closure at WebAssembly level, the original
  function arguments aren't used as mutable values since they are in memory anyway (so no benefit).
  Instead create the mutable locals and set them to the inital arguments at the start.
  Because they are locals that aren't shared (for single recursive fun),
  don't actually need args and _args to be distinct.
  Just do args = args* at start, and args = args' in tail call. Safe as nothing else uses the Assign operation.

  TODO: Since continue is false at end of function, and set just before it is checked when it is needed,
        can share across ALL tail recursive functions. Can do the same for result.
        Or is there no point as they can be kept as locals otherwise, rather than putting in memory.
        (Continue/args and 'next' must be shared when mutually recursive functions supported)
*)

(* Rewrite tail recursive call *)
let rewrite_tail_call continue_id args new_args =
  binds_to_anf
     (List.map (fun (arg, new_arg) -> BEffect(Compound.assign arg new_arg)) (List.combine args new_args))
     (LinastExpr.compound (Compound.assign continue_id (Imm.const (Const_int 1))))

(* Search everywhere a tail recursive call for the given function could occur and replace it *)
let rec rewrite_body f_id continue_id args linast = match linast.desc with
  | LLetRec (binds, rest) ->
    {linast with desc = LLetRec(binds, rewrite_body f_id continue_id args rest)}
  | LLet (id, global, bind, rest) ->
    {linast with desc = LLet(id, global, bind, rewrite_body f_id continue_id args rest)}
  | LSeq (c, rest) ->
    {linast with desc = LSeq(c, rewrite_body f_id continue_id args rest)}
  (* Tail call to replace *)
  | LCompound {desc = CApp({desc = ImmIdent f}, new_args); annotations}
     when Ident.same f_id f && List.mem TailCall (!annotations) ->
     rewrite_tail_call continue_id args new_args
  (* Recurse on any Linasts within the compound term *)
  | LCompound c -> {linast with desc = LCompound(rewrite_compound f_id continue_id args c)}

and rewrite_compound f_id continue_id args (compound : compound_expr) = match compound.desc with
  | CSwitch (imm, cases, default) ->
    {compound with desc =
      CSwitch(imm,
        List.map (fun (i, body) -> (i, rewrite_body f_id continue_id args body)) cases,
        Option.map (rewrite_body f_id continue_id args) default)}

  | CIf (imm, body1, body2) ->
      {compound with desc =
        CIf(imm,
          rewrite_body f_id continue_id args body1,
          rewrite_body f_id continue_id args body2)}

  | CMatchTry (imm, body1, body2) ->
      {compound with desc =
        CMatchTry(imm,
          rewrite_body f_id continue_id args body1,
          rewrite_body f_id continue_id args body2)}
  | _ -> compound

(* Redefine function to be tail recursive *)
let rewrite_function f_id args body =
  (* Create new formal parameters for the function, as the original paramters
     will now be used as mutable locals rather than coming from function argument/closure *)
  let new_params = List.map (fun id -> Ident.create_local (Ident.name id)) args in
  let initial_binds = List.map (fun (arg, param) -> BLet(arg, Compound.imm (Imm.id param)))
    (List.combine args new_params) in
  let continue_id = Ident.create_local "continue"
  and result_id = Ident.create_local "result_mut"
  (* Need to bind the result to an immediate before LAssign can be used *)
  and temp_result_id = Ident.create_local "result" in

  let while_body =
    binds_to_anf [BEffect(Compound.assign continue_id (Imm.const (Const_int 0)))]
    (* Need to rewrite the body of the function so that it binds the result to 'result'.
       Uses 'rewrite_tree' previously defined in optConstants for pulling out guarenteed branches  *)
    (* Since CAssign takes an imm, first need to bind the result to a 'temp_reuslt_id' *)
    (OptConstants.rewrite_tree (LinastExpr.mklet temp_result_id Local)
      (rewrite_body f_id continue_id args body)
      (LinastExpr.compound (Compound.assign result_id (Imm.id temp_result_id)))) in

  (* Note - each ident has to first be assigned to by a Let expression, before LAssign can be used *)
  let new_body =
    (* Only place in the whole project ~mut is used. Custom function might be better? *)
    binds_to_anf ~mut:(continue_id :: result_id :: args)
    ((BLet(continue_id, Compound.imm (Imm.const (Const_int 1)))) ::
     (BLet(result_id, Compound.imm unit_value)) ::
     initial_binds @
     [BEffect(Compound.mkwhile (LinastExpr.compound (Compound.imm (Imm.id continue_id))) while_body)])
    (LinastExpr.compound (Compound.imm (Imm.id result_id))) in
  (* Returns a new function, so TailRecursive annotation and any others are removed *)
  Compound.mkfun new_params new_body

(* Scan the whole program and rewrite each tail recursive function, replacing recursive calls within it *)
(* Arbitrarily chose to replace in bottom up manner *)
let leave_linast linast = match linast.desc with
  (* TODO: Handle mutually recursive functions as well as simple recursive functions *)
  | LLetRec ([id, global, f], rest) when List.mem TailRecursive (!(f.annotations)) ->
    (match f.desc with
      | CFunction(args, body) ->
        let new_f = rewrite_function id args body in
        {linast with desc = LLetRec([id, global, new_f], rest)}
      | _ -> failwith "LetRec binding wrongly marked as being tail recursive fun")
  | _ -> linast

let optimise linast =
  analyse_program linast;
  (LinastMap.create_mapper ~leave_linast ()) linast
