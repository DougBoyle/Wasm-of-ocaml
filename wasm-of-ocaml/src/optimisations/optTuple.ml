(* When the only place a local function occurs is in full/over applications, and it has more than
   1 argument, replace it with a function accepting a tuple.
   TODO: Make use of callKnown to avoid having put put arguments into a tuple before calling *)
open Linast
open LinastUtils


let is_not_f f = function
  | ({desc=ImmIdent id} : imm_expr) -> not (Ident.same f id)
  | _ -> true
let is_f f = function
  | ({desc=ImmIdent id} : imm_expr) -> Ident.same f id
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
   Also able to make use of fact that program is well typed, so can't do Field/ArrayGet on a function *)
and safe_to_rewrite_compound f arity (compound : compound_expr) = match compound.desc with
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

(* Written this way since rewriting introduces at least 1 let binding to create the block,
   so needs to operate on the linast *)
let rewrite_call f arity linast = match linast.desc with
  | LLet(id, local, ({desc=CApp(imm, args)} as compound), rest) when is_f f imm && List.length args = arity ->
    let tuple = Ident.create_local "tuple" in
    binds_to_anf
      [BLet(tuple, Compound.makeblock 0 args)]
      {linast with desc=LLet(id, local, {compound with desc=CApp(imm, [Imm.id tuple])}, rest)}
  | LLet(id, local, ({desc=CApp(imm, args)} as compound), rest) when is_f f imm && List.length args > arity ->
    let applied, remaining = Utils.take arity args in
    let tuple = Ident.create_local "tuple" in
    let result = Ident.create_local "result" in
    binds_to_anf
      [BLet(tuple, Compound.makeblock 0 applied);
       BLet(result, Compound.app imm [Imm.id tuple])]
      {linast with desc=LLet(id, local, {compound with desc=CApp(Imm.id result, remaining)}, rest)}

  (* LetRec can only be used to define functions, can't be an application *)
  | LSeq(({desc=CApp(imm, args)} as compound), rest) when is_f f imm && List.length args = arity ->
    let tuple = Ident.create_local "tuple" in
    binds_to_anf
      [BLet(tuple, Compound.makeblock 0 args)]
      {linast with desc=LSeq({compound with desc=CApp(imm, [Imm.id tuple])}, rest)}
  | LSeq(({desc=CApp(imm, args)} as compound), rest) when is_f f imm && List.length args > arity ->
    let applied, remaining = Utils.take arity args in
    let tuple = Ident.create_local "tuple" in
    let result = Ident.create_local "result" in
    binds_to_anf
      [BLet(tuple, Compound.makeblock 0 applied);
       BLet(result, Compound.app imm [Imm.id tuple])]
      {linast with desc=LSeq({compound with desc=CApp(Imm.id result, remaining)}, rest)}

  | LCompound ({desc=CApp(imm, args)} as compound) when is_f f imm && List.length args = arity ->
    let tuple = Ident.create_local "tuple" in
    binds_to_anf
      [BLet(tuple, Compound.makeblock 0 args)]
      {linast with desc=LCompound {compound with desc=CApp(imm, [Imm.id tuple])}}
  | LCompound ({desc=CApp(imm, args)} as compound) when is_f f imm && List.length args > arity ->
    let applied, remaining = Utils.take arity args in
    let tuple = Ident.create_local "tuple" in
    let result = Ident.create_local "result" in
    binds_to_anf
      [BLet(tuple, Compound.makeblock 0 applied);
       BLet(result, Compound.app imm [Imm.id tuple])]
      {linast with desc=LCompound {compound with desc=CApp(Imm.id result, remaining)}}
  | _ -> linast

let rewrite_function args body =
  let tuple = Ident.create_local "tuple" in
  Compound.mkfun [tuple]
  (binds_to_anf
    (List.mapi (fun i id -> BLet(id, Compound.field (Imm.id tuple) i)) args)
    body)

let enter_linast linast = match linast.desc with
  | LLet(id, Local, {desc=CFunction(args, body)}, rest) ->
    let num_args = List.length args in
    if num_args > 1 && safe_to_rewrite id num_args rest then
      let rewriter = LinastMap.create_mapper ~enter_linast:(rewrite_call id num_args) () in
      {linast with desc=LLet(id, Local, rewrite_function args body, rewriter rest)}
    else linast
  (* Handle LLetRec by just processing one at a time *)
  | LLetRec(binds, rest) ->
    let replacements =
     List.fold_right (fun (id, local, (compound : compound_expr)) replaced ->
      match local, compound.desc with
        | Local, CFunction(args, body) ->
          let num_args = List.length args in
          if num_args > 1 && safe_to_rewrite id num_args linast then
          (id, (num_args, rewrite_function args body))::replaced else replaced
        | _ -> replaced
      ) binds [] in
    if replacements = [] then linast else
    (* Rewrite each function definition *)
    let new_binds = List.map (fun (id, local, compound) ->
      match List.assoc_opt id replacements with
        | None -> (id, local, compound)
        | Some (_, new_fun) -> (id, local, new_fun)
      ) binds in
    (* Rewrite the applications for each function *)
    List.fold_right (fun (id, (arity, _)) linast ->
      (LinastMap.create_mapper ~enter_linast:(rewrite_call id arity) ()) linast
    ) replacements {linast with desc=LLetRec(new_binds, rest)}
  | _ -> linast

let optimise linast =
  (LinastMap.create_mapper ~enter_linast ()) linast

