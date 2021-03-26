(* When the only place a local function occurs is in full/over applications, and it has more than
   1 argument, replace it with a function accepting a tuple. *)
open Linast
open LinastUtils


let is_not_f f = function
  | {i_desc=ImmIdent id} -> not (Ident.same f id)
  | _ -> true
let is_f f = function
  | {i_desc=ImmIdent id} -> Ident.same f id
  | _ -> false

let rec safe_to_rewrite f arity linast = match linast.desc with
  | LLetRec(binds, rest) ->
    List.for_all (fun (_, _, bind) -> safe_to_rewrite_compound f arity bind) binds &&
    safe_to_rewrite f arity rest
  | LLet(_, _, compound, rest) | LSeq(compound, rest) ->
    safe_to_rewrite_compound f arity compound && safe_to_rewrite f arity rest
  | LCompound compound -> safe_to_rewrite_compound f arity compound

(* The only variables passed out of try blocks are ones bound by patterns, so not
   a function declared in a let binding unless it was used elsewhere already.
   Program is well typed, so also can't do Field/ArrayGet on a function *)
and safe_to_rewrite_compound f arity compound = match compound.c_desc with
  | CImm imm | CSetField(_, _, imm) | CArraySet(_, _, imm) | CAssign(_, imm) ->
    is_not_f f imm
  | CMatchFail _ | CField _ | CArrayGet _ | CGetTag _ | CUnary _ | CBinary _ -> true
  | CMakeBlock (_, imms) -> List.for_all (is_not_f f) imms
  | CIf (_, body1, body2) | CWhile(body1, body2) | CMatchTry(_, body1, body2) ->
    safe_to_rewrite f arity body1 && safe_to_rewrite f arity body2
  | CFunction (_, body) | CFor (_, _, _, _, body) -> safe_to_rewrite f arity body
  | CSwitch (_, cases, default) ->
    List.for_all (fun (_, body) -> safe_to_rewrite f arity body) cases &&
    (match default with None -> true | Some body -> safe_to_rewrite f arity body)
  | CApp (imm, args) ->
    List.for_all (is_not_f f) args &&
    (is_not_f f imm || List.length args >= arity) (* An application of the tupled function we can optimise *)

(* Only need to rewrite over-applied functions, where tupled and curried arguments need separating *)
let rewrite_call f arity linast = match linast.desc with
  | LLet(id, local, ({c_desc=CApp(imm, args)} as compound), rest) when is_f f imm && List.length args > arity ->
    let applied, remaining = Utils.take arity args in
    let result = Ident.create_local "result" in
    binds_to_anf
       [BLet(result, Compound.app imm applied)]
      {linast with desc=LLet(id, local, {compound with c_desc=CApp(Imm.id result, remaining)}, rest)}

  (* LetRec can only be used to define functions, can't be an application *)
  | LSeq(({c_desc=CApp(imm, args)} as compound), rest) when is_f f imm && List.length args > arity ->
   let applied, remaining = Utils.take arity args in
   let result = Ident.create_local "result" in
   binds_to_anf
     [BLet(result, Compound.app imm applied)]
     {linast with desc=LSeq({compound with c_desc=CApp(Imm.id result, remaining)}, rest)}

  | LCompound ({c_desc=CApp(imm, args)} as compound) when is_f f imm && List.length args > arity ->
    let applied, remaining = Utils.take arity args in
    let result = Ident.create_local "result" in
    binds_to_anf
      [BLet(result, Compound.app imm applied)]
      {linast with desc=LCompound {compound with c_desc=CApp(Imm.id result, remaining)}}
  | _ -> linast

let mark_tupled compound =
  compound.c_annotations := Tupled :: (!(compound.c_annotations)); compound

let enter_linast linast = match linast.desc with
  | LLet(id, Local, ({c_desc=CFunction(args, body)} as compound), rest) ->
    let num_args = List.length args in
    if num_args > 1 && safe_to_rewrite id num_args rest then
      let rewriter = LinastMap.create_mapper ~enter_linast:(rewrite_call id num_args) () in
      {linast with desc=LLet(id, Local, mark_tupled compound, rewriter rest)}
    else linast
  (* Handle LLetRec by just processing one at a time *)
  | LLetRec(binds, rest) ->
    let replacements =
     List.fold_right (fun (id, local, compound) replaced ->
      match local, compound.c_desc with
        | Local, CFunction(args, body) ->
          let num_args = List.length args in
          if num_args > 1 && safe_to_rewrite id num_args linast then
          (id, num_args)::replaced else replaced
        | _ -> replaced
      ) binds [] in
    if replacements = [] then linast else
    (* Rewrite each function definition *)
    let new_binds = List.map (fun (id, local, compound) ->
      match List.assoc_opt id replacements with
        | None -> (id, local, compound)
        | Some _ -> (id, local, mark_tupled compound)
      ) binds in
    (* Rewrite the applications for each function *)
    List.fold_right (fun (id, arity) linast ->
      (LinastMap.create_mapper ~enter_linast:(rewrite_call id arity) ()) linast
    ) replacements {linast with desc=LLetRec(new_binds, rest)}
  | _ -> linast

let optimise linast =
  (LinastMap.create_mapper ~enter_linast ()) linast

