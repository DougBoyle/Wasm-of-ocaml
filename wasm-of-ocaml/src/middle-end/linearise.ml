open Typedtree
open Linast
open Types
open CompileMatch
open LinastUtils

(* May end up needing different functions for translating to linast vs compound vs imm expr *)
(* Can encode quite nicely as a value of required type accompanied with a set of needed linast bindings (to do left-to-right) *)

(* A list equivalent to a Linast: Collect a sequence of the bindings needed as we translate things
   then merge them into a Linast tree at the end. *)

let rec take n l = if n = 0 then ([], l) else
  match l with [] -> assert false (* Should never happen *) | x::xs -> let (h, t) = take (n-1) xs in (x::h, t)


let translate_ident path = function
  | Val_prim p -> Primitives.translate_prim p
  (* May need to handle some identifiers which point directly to runtime functions e.g. Stdlib!.min
     Could also just 'hack' these into the file as usual idents but would be inefficient and probably slower. *)
  | _ -> (match path with
    | Path.Pident id -> id
    | Path.Pdot(Path.Pident (Ident.Global "Stdlib"), s) -> Primitives.translate_other_prim s
    | _ -> raise NotImplemented)

(*
let translate_ident = function
  | Path.Pident id -> id
  (* All primitives are identified in this way. Checking val_kind = Val_prim p doesn't catch all cases *)
  | Path.Pdot(Path.Pident (Ident.Global "Stdlib"), s) -> Primitives.translate_prim s
  | _ -> raise NotSupported
  *)

let get_id_from_pattern = function
    | {pat_desc=Tpat_var(id, _)} -> id
    | _ -> raise NotSupported

(* TODO: Texp_extension_constructor, Texp_array and Texp_variant all left out initially, may implement later once working.
         Also going to leave Texp_letop for now - can implement programmatically but again, very niche use so don't do in MVP. *)
(* Keep refering to translCore to catch special cases of labelled args, primitives, etc. *)
(* TODO: Can simplify in some cases and call correct level and just use its result
         e.g. Tuple - make call to translate_compound then just assign that to a variable name. *)
let rec translate_imm ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) =
  match exp_desc with
  |  Texp_ident (path, idLoc, valDesc) -> (Imm.id (translate_ident path valDesc.val_kind), [])

  | Texp_constant c -> (Imm.const c, [])

  (* Can probably rewrite as a fold *)
  | Texp_let(Nonrecursive, [], e) -> translate_imm e
  | Texp_let(Nonrecursive, {vb_pat;vb_expr}::rest, body) ->
      let (exp, exp_setup) = translate_compound vb_expr in
      let bindList = getBindings fail_trap vb_pat exp in
      let (rest, rest_setup) = translate_imm ({e with exp_desc=Texp_let(Nonrecursive, rest, body)}) in
      (rest, exp_setup @ bindList @ rest_setup)

  | Texp_let(Recursive, binds, body) ->
    let (binds, binds_setup) =
      List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, translate_compound vb_expr)) binds) in
    let (required_binds, required_binds_setup) = List.split binds_setup in
    let names = List.map (function
        | {pat_desc=Tpat_var(id, _)} -> id
        | _ -> raise NotSupported (* LHS of let rec must be a function identifier *)) binds in
    let (body, body_setup) = translate_imm body in
    (body, (List.concat required_binds_setup)
           @ [BLetRec (List.combine names required_binds)]
           @ body_setup)

  | Texp_sequence (e1, e2) ->
    let (effect, effect_setup) = translate_compound e1 in
    let (result, result_setup) = translate_imm e2 in
      (result, effect_setup @ [BEffect effect] @ result_setup)

  | Texp_tuple _ | Texp_construct _ | Texp_record _ | Texp_field _
  | Texp_setfield _ | Texp_ifthenelse _ | Texp_while _ | Texp_for _
  | Texp_function _ | Texp_apply _ | Texp_match _ ->
    let id = Ident.create_local "compound" in
    let (compound, setup) = translate_compound e in
    (Imm.id id, setup @ [BLet(id, compound)])

  | Texp_letop {let_; ands; param; body; partial} -> raise NotImplemented
  | _ -> raise NotSupported

(* TAKEN FROM OCAML COMPILER Lambda/Translcore.ml *)
(* Can't see why optional arguments matter? Just due to pre-evaluating supplied ones? *)
(* TODO: Not fully handling optional/labelled args, differs from how OCaml compiler handles transl_apply *)
(* Returns a compound expr *)
(* f already compiled to compound *)
and transl_apply f args =
  let rec getSetupAndImms = function
    | [] -> ([], [])
    | (_, None) :: rest -> let (args, setup) = getSetupAndImms rest in ((None::args), setup)
    | (_, Some e) :: rest -> let (im, s) = translate_imm e in
      let (args, setup) = getSetupAndImms rest in ((Some im) :: args, s @ setup)
  in let (args, argsetup) = getSetupAndImms args in
(*  let (f, fsetup) = translate_compound f in *)

  let flattenApp (f : compound_expr) args = match f.desc with
    | CApp(f', args') -> ({f with desc=CApp(f', args' @ args)}, [])
    | _ -> let id = Ident.create_local "fun" in (Compound.app (Imm.id id) args, [BLet(id, f)])

  (* Convert args into two lists - new variables to abstract over (skipped labelled vars) and whole list of applications *)
  in let rec getAbstractionsAndApplications = function
    | [] -> ([], [])
    | (Some x)::args -> let (ab, ap) = getAbstractionsAndApplications args in (ab, x :: ap)
    | None::args -> let (ab, ap) = getAbstractionsAndApplications args in
      let id = Ident.create_local "param" in
      let imm = Imm.id id in (id::ab, imm::ap)
  in let (ab, ap) = getAbstractionsAndApplications args in
  let (newF, newSetup) = flattenApp f ap in

  match ab with [] -> (newF, argsetup @ newSetup) (* The usual obvious case *)
    | _ -> (Compound.mkfun ab (LinastExpr.compound newF), argsetup @ newSetup)
  (* TODO: Check all setup expressions carried through to final return value *)

and translate_compound ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) =
  match exp_desc with
  |  Texp_ident (path, idLoc, valDesc) -> (Compound.imm (Imm.id (translate_ident path valDesc.val_kind)), [])

  | Texp_constant c -> (Compound.imm (Imm.const c), [])

  (* Can probably rewrite as a fold *)
  | Texp_let(Nonrecursive, [], e) -> translate_compound e
  | Texp_let(Nonrecursive, {vb_pat;vb_expr}::rest, body) ->
      let (exp, exp_setup) = translate_compound vb_expr in
      let bindList = getBindings fail_trap vb_pat exp in
      let (rest, rest_setup) = translate_compound ({e with exp_desc=Texp_let(Nonrecursive, rest, body)}) in
      (rest, exp_setup @ bindList @ rest_setup)

  | Texp_let(Recursive, binds, body) ->
      (* TODO: Why isn't it an issue that the setup of one expression may call one of the other recursive functions, hence
               that setup surely can't come before the recursive function? *)
      let (binds, binds_setup) =
        List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, translate_compound vb_expr)) binds) in
      let (required_binds, required_binds_setup) = List.split binds_setup in
      let names = List.map (function
          | {pat_desc=Tpat_var(id, _)} -> id
          | _ -> raise NotSupported (* LHS of let rec must be a function identifier *)) binds in
      let (body, body_setup) = translate_compound body in
      (body, (List.concat required_binds_setup)
               @ (BLetRec (List.combine names required_binds)) :: body_setup)

  (* TODO: Refactor translate_imm to just call these *)
  | Texp_tuple l ->
    let (args, setup) = List.split (List.map translate_imm l) in
    (Compound.makeblock 0l args, List.concat setup)

  | Texp_construct (identLoc, desc, l) ->
    let (args, setup) = List.split (List.map translate_imm l) in
    (Compound.makeblock (unify_constructor_tag desc.cstr_tag) args, List.concat setup)

 (* Made easier by fact that Typedtree always puts fields in order, regardless of order in program.
    Hence no need to do sorting or check label descriptions *)
 (* TODO: Having to convert arrays to lists suggests I should represent things slightly differently here *)
  | Texp_record {fields; extended_expression} ->
    let id = Ident.create_local "record" in
    (match extended_expression with
    (* Not built off of anything so each field must be Overridden *)
    | None -> let (args, setup) =
        List.split (List.map (function (_, Overridden(_, e)) -> translate_imm e | _ -> raise NotSupported) (Array.to_list fields))
      in (Compound.makeblock 0l args, List.concat setup)
    | Some e -> let extract_field original i = (function
         | (_, Overridden(_, e)) -> translate_imm e
         | (_, Kept _) -> let fieldId = Ident.create_local "field" in
           (Imm.id fieldId, [BLet(id, Compound.field original (Int32.of_int i))]))
       in let (original, original_setup) = translate_imm e in
       let (args, setup) = List.split (List.mapi (extract_field original) (Array.to_list fields))
       in (Compound.makeblock 0l args, original_setup @ (List.concat setup))
    )

  | Texp_field (e, identLoc, labelDesc) ->
    let (record, setup) = translate_imm e in
    (Compound.field record (Int32.of_int labelDesc.lbl_pos), setup)

  | Texp_setfield (e, identLoc, labelDesc, v) ->
    let (record, record_setup) = translate_imm e in
    let (value, value_setup) = translate_imm v in
    (Compound.setfield record (Int32.of_int labelDesc.lbl_pos) value, record_setup @ value_setup)

  | Texp_ifthenelse (e1, e2, e3opt) ->
    let e3_lin = (match e3opt with Some e -> translate_linast e
      (* TODO: Use a single global unit, or treat as int not block *)
      | None -> LinastExpr.compound (Compound.makeblock 0l [])) in
    let (e1_imm, e1_setup) = translate_imm e1 in
    let e2_lin = translate_linast e2 in
    (Compound.mkif e1_imm e2_lin e3_lin, e1_setup)

  | Texp_sequence (e1, e2) ->
    let (effect, effect_setup) = translate_compound e1 in
    let (result, result_setup) = translate_compound e2 in
    (result, effect_setup @ (BEffect effect)::result_setup)

  | Texp_while (e1, e2) ->
      let (test, test_setup) = translate_imm e1 in
      let loop = translate_linast e2 in
      (Compound.mkwhile test loop, test_setup)
  (* Translcore in OCaml doesn't use the second pattern arg, just annotation information *)
  | Texp_for (param, _, start, finish, dir, e) ->
    let (start, start_setup) = translate_imm start in
    let (finish, finish_setup) = translate_imm finish in
    let expr = translate_linast e in
    (Compound.mkfor param start finish dir expr, start_setup @ finish_setup)

  (* TODO: Look at how translprim collapses down curried functions. Each Texp_function only has 1 argument *)
  | Texp_function { param; cases; partial; } ->
    (match cases with [{c_lhs=pat; c_guard=None;
     c_rhs={exp_desc = Texp_function { arg_label = _; param = param'; cases;
     partial = partial'; }; exp_env; exp_type} as exp}]
     (* Pattern always matches and never binds anything *)
     (* Find the function for the inner body and attach param on front *)
     when Parmatch.inactive ~partial pat ->
       (match translate_compound exp with
         | ({desc=CFunction(args, body);_} as comp, setup) ->
           ({comp with desc=CFunction(param::args, body)}, setup)
         | _ -> assert false (* Know body is a Texp_function, so recursive call should always return a function *)
       )
   | _ ->
    let (comp, setup) = compile_match partial fail_trap (Compound.imm (Imm.id param)) (transl_cases cases) in
    (Compound.mkfun [param] (binds_to_anf setup (LinastExpr.compound comp)), [])
   )

  (* Fully applied primitive *)
  (* TODO: Change to correctly detect primitives *)
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p}); exp_type = prim_type }, oargs)
      when List.length oargs >= p.prim_arity && List.for_all (fun (_, arg) -> arg <> None) oargs ->
        let argl, extra_args = take p.prim_arity oargs in
        let arg_exps =
           List.map (function _, Some x -> x | _ -> assert false) argl
        in
        let (op, arg_setups) = translate_prim_app p arg_exps
        in if extra_args = [] then (op, arg_setups)
        else let (app, setup) = transl_apply op extra_args in
        (app, arg_setups @ setup)

  | Texp_apply (f, args) ->
    let (f_compound, fsetup) = translate_compound f in
    let (app, setup) = transl_apply f_compound args in (app, fsetup @ setup)

  | Texp_match (e, cases, partial) ->
   let cases = List.map (fun case -> match split_pattern case.c_lhs with
      | (Some p, None) -> {case with c_lhs=p}
      | _ -> raise NotSupported (* No exception patterns allowed *)) cases in
   let (arg, arg_setup) = translate_imm e in
   let (body, setup) = compile_match partial fail_trap (Compound.imm arg) (transl_cases cases) in
   (body, arg_setup @ setup)

  | Texp_letop {let_; ands; param; body; partial} -> raise NotImplemented
  | _ -> raise NotSupported

and transl_cases cases =
   List.map (fun {c_lhs;c_guard;c_rhs} -> (c_lhs, translate_compound c_rhs, Option.map translate_compound c_guard)) cases

and translate_linast ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) =
  let (compound, setup) = translate_compound e in binds_to_anf setup (LinastExpr.compound compound)

(* AND/OR handled specially as they sometimes don't evaluate their second argument *)
and translate_prim_app (primDesc : Primitive.description) args =
      match (Hashtbl.find Primitives.prim_table primDesc.prim_name, args) with
        | Unary unop, [arg] -> let (imm, setup) = translate_imm arg in
          (Compound.unary unop imm, setup)
        | Binary AND, [arg1; arg2] -> let (imm1, setup) = translate_imm arg1 in
          (Compound.mkif imm1 (translate_linast arg2) (LinastExpr.compound (Compound.imm imm1)), setup)
        | Binary OR, [arg1; arg2] -> let (imm1, setup) = translate_imm arg1 in
          (Compound.mkif imm1 (LinastExpr.compound (Compound.imm imm1)) (translate_linast arg2), setup)
        | Binary binop, [arg1; arg2] -> let (imm1, setup1) = translate_imm arg1 in
          let (imm2, setup2) = translate_imm arg2 in
          (Compound.binary binop imm1 imm2, setup1 @ setup2)
        | _ -> assert false (* Should never be possible to get an arity mismatch here *)

let rec get_idents = function
  | [] -> []
  | item::rest -> (
    match item.str_desc with
      | Tstr_value (_, binds) -> List.rev_append (let_bound_idents binds) (get_idents rest)
      | _ -> get_idents rest
      (* TODO: Check no more cases to consider *)
  )

let rec getExports (tree, coercion) =
  let idents = get_idents tree in
  match coercion with
    | Tcoerce_none -> idents
    | Tcoerce_structure (poslist, _) ->
      (* Subcomponents should always be Tcoerce_none. No module so single layer *)
      List.map (fun (i, _) -> List.nth idents i) poslist
    | _ -> raise NotSupported

let rec translate_structure exported = function
 |  [] -> LinastExpr.compound (Compound.makeblock 0l [])
 | item::items -> (match item.str_desc with
   | Tstr_eval (e, _) -> let (compound, setup) = translate_compound e in
     binds_to_anf setup (LinastExpr.seq compound (translate_structure exported items))
   | Tstr_value (Recursive, bind_list) ->
    let binds, setups = List.fold_right (fun {vb_pat; vb_expr;} (binds, setups) ->
      let id = get_id_from_pattern vb_pat in
      let (compound, setup) = translate_compound vb_expr in
      ((id, (if (List.exists (fun x -> x = id) exported) then Global else Local), compound)::binds, setup@setups)
    ) bind_list ([], [])
    in binds_to_anf setups (LinastExpr.mklet Recursive binds (translate_structure exported items))
   | Tstr_value (Nonrecursive, []) -> translate_structure exported items
   (* TODO: Should have 'pre-anf' (see Grain) to simplify what can appear in this type of let binding *)
   | Tstr_value (Nonrecursive, {vb_pat;vb_expr;}::bind_list) ->
     let (compound, compound_setup) = translate_compound vb_expr in
     let binds = getBindings fail_trap vb_pat compound in
     binds_to_anf ~exported (compound_setup @ binds) (translate_structure exported items)
   | _ -> translate_structure exported items (* TODO: Should check which ones should/shouldn't be included *)
  )


let translate_structure_with_coercions (structure, coercions) =
 binds_to_anf (!primBinds) (translate_structure (getExports (structure.str_items, coercions)) structure.str_items)
 (* Add on primitive definitions *)