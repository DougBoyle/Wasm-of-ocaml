open Typedtree
open Linast
open Types
open CompileMatch
open LinastUtils

let rec take n l = if n = 0 then ([], l) else
  match l with [] -> assert false (* Should never happen *) | x::xs -> let (h, t) = take (n-1) xs in (x::h, t)

let translate_ident path = function
  | Val_prim p -> Imm.id (Primitives.translate_prim p)
  (* May need to handle some identifiers which point directly to runtime functions e.g. Stdlib!.min
     Could also just 'hack' these into the file as usual idents but would be inefficient and probably slower. *)
  | _ -> (match path with
    | Path.Pident id -> Imm.id id
    | Path.Pdot(Path.Pident (Ident.Global "Stdlib"), s) -> Primitives.translate_other_prim s
    | _ -> raise (NotImplemented __LOC__))

let get_id_from_pattern = function
    | {pat_desc=Tpat_var(id, _)} -> id
    | _ -> raise NotSupported

(* TODO: Texp_extension_constructor and Texp_variant all left out initially, may implement later once working. *)
let rec translate_imm ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) =
  match exp_desc with
  |  Texp_ident (path, idLoc, valDesc) -> (translate_ident path valDesc.val_kind, [])

  | Texp_constant c -> (Imm.const c, [])

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

  | Texp_construct (identLoc, desc, []) when desc.cstr_nonconsts = 0 ->
    (Imm.const (Asttypes.Const_int (get_const_constructor_tag desc.cstr_tag)), [])

  | Texp_tuple _ | Texp_construct _ | Texp_record _ | Texp_field _
  | Texp_setfield _ | Texp_ifthenelse _ | Texp_while _ | Texp_for _
  | Texp_function _ | Texp_apply _ | Texp_match _ | Texp_array _
  | Texp_let(Nonrecursive, _, _) ->
    let id = Ident.create_local "compound" in
    let (compound, setup) = translate_compound e in
    (Imm.id id, setup @ [BLet(id, compound)])

  | Texp_letop {let_; ands; param; body; partial} -> raise (NotImplemented __LOC__)
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

  (* TODO: Can't currently curry functions in this way, so unroll them *)
  match ab with [] -> (newF, argsetup @ newSetup) (* The usual obvious case *)
    | _ ->
    (List.fold_right
    (fun id expr -> Compound.mkfun [id] (LinastExpr.compound expr)) ab newF,
    argsetup @ newSetup)
  (*  (Compound.mkfun ab (LinastExpr.compound newF), argsetup @ newSetup)  *)
  (* TODO: Check all setup expressions carried through to final return value *)

and translate_compound ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) =
  match exp_desc with
  |  Texp_ident (path, idLoc, valDesc) -> (Compound.imm (translate_ident path valDesc.val_kind), [])

  | Texp_constant c -> (Compound.imm (Imm.const c), [])

  (* Can probably rewrite as a fold *)
  | Texp_let(Nonrecursive, [], e) -> translate_compound e
  | Texp_let(Nonrecursive, {vb_pat;vb_expr}::rest, body) ->
    let ((rest : Linast.compound_expr), rest_setup) = translate_compound ({e with exp_desc=Texp_let(Nonrecursive, rest, body)}) in
      let matrix = [([vb_pat], ((rest, rest_setup), []), None)] in
      let (value, value_setup) = translate_imm vb_expr in
      let (body, setup) = compile_matrix fail_trap [value] matrix in
      (body, value_setup @ setup)

  | Texp_let(Recursive, binds, body) ->
      (* Most potential recursion issues solved by the constrained form allowed for letrec expressions *)
      let (binds, binds_setup) =
        List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, translate_compound vb_expr)) binds) in
      let (required_binds, required_binds_setup) = List.split binds_setup in
      let names = List.map (function
          | {pat_desc=Tpat_var(id, _)} -> id
          | _ -> raise NotSupported (* LHS of let rec must be a function identifier *)) binds in
      let (body, body_setup) = translate_compound body in
      (body, (List.concat required_binds_setup)
               @ (BLetRec (List.combine names required_binds)) :: body_setup)

  | Texp_tuple l ->
    let (args, setup) = List.split (List.map translate_imm l) in
    (Compound.makeblock 0l args, List.concat setup)

  | Texp_construct (identLoc, desc, []) when desc.cstr_nonconsts = 0 ->
    (Compound.imm (Imm.const (Asttypes.Const_int (get_const_constructor_tag desc.cstr_tag))), [])
  | Texp_construct (identLoc, desc, l) ->
    let (args, setup) = List.split (List.map translate_imm l) in
    (Compound.makeblock (unify_constructor_tag desc) args, List.concat setup)
  (* TODO: Add type annotations to the arguments to indicate mutable/immutable (all mut for arrays) *)
  | Texp_array l -> let (args, setup) = List.split (List.map translate_imm l) in
    (Compound.makeblock (Int32.of_int (List.length l)) args, List.concat setup)

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
      | None -> LinastExpr.compound (Compound.imm unit_value)) in
    let (e1_imm, e1_setup) = translate_imm e1 in
    let e2_lin = translate_linast e2 in
    (Compound.mkif e1_imm e2_lin e3_lin, e1_setup)

  | Texp_sequence (e1, e2) ->
    let (effect, effect_setup) = translate_compound e1 in
    let (result, result_setup) = translate_compound e2 in
    (result, effect_setup @ (BEffect effect)::result_setup)

  | Texp_while (e1, e2) ->
      let test = translate_linast e1 in
      let loop = translate_linast e2 in
      (Compound.mkwhile test loop, [])
  (* Translcore in OCaml doesn't use the second pattern arg, just annotation information *)
  | Texp_for (param, _, start, finish, dir, e) ->
    let (start, start_setup) = translate_imm start in
    let (finish, finish_setup) = translate_imm finish in
    let expr = translate_linast e in
    (Compound.mkfor param start finish dir expr, start_setup @ finish_setup)

  (* TODO: Look at how translprim collapses down curried functions. Each Texp_function only has 1 argument *)
  (* Rather than special case for currying, could add a special case for tuples. *)
  | Texp_function { param; cases; partial; } ->
    translate_function param cases partial

  (* Fully applied primitive *)
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p})}, oargs)
      when List.length oargs >= p.prim_arity && List.for_all (fun (_, arg) -> arg <> None) oargs ->
        let argl, extra_args = take p.prim_arity oargs in
        let arg_exps = List.map (function _, Some x -> x | _ -> assert false) argl in
        let (op, arg_setups) = translate_prim_app p arg_exps
        in if extra_args = [] then (op, arg_setups)
        else let (app, setup) = transl_apply op extra_args in
        (app, arg_setups @ setup)
  (* Not 'Val_prim primitive_description' so have to extract arity manually. val_kind = Val_reg to exclude Val_prim cases *)
  | Texp_apply({exp_desc = Texp_ident(Path.Pdot(Path.Pident (Ident.Global "Stdlib"), name), _, {val_kind = Val_reg})}, oargs)
    when List.length oargs >= (match Hashtbl.find Primitives.prim_table name with Unary _ -> 1 | Binary _ -> 2)
         && List.for_all (fun (_, arg) -> arg <> None) oargs ->
    let op, setup, extra_args = (match (Hashtbl.find Primitives.prim_table name, oargs) with
      | Unary unop, (_, Some arg)::extra_args ->
        let (arg_imm, setup) = translate_imm arg in
        let op = Compound.unary unop arg_imm in (op, setup, extra_args)
      | Binary binop, (_, Some arg1)::(_, Some arg2)::extra_args ->
        let (arg1_imm, setup1) = translate_imm arg1 in
        let (arg2_imm, setup2) = translate_imm arg2 in
        let op = Compound.binary binop arg1_imm arg2_imm in (op, setup1 @ setup2, extra_args)
      | _ -> failwith "Incorrect guard statement") in
    if extra_args = [] then (op, setup) else let (app, rest_setup) = transl_apply op extra_args in (app, setup @ rest_setup)

  | Texp_apply (f, args) ->
    let (f_compound, fsetup) = translate_compound f in
    let (app, setup) = transl_apply f_compound args in (app, fsetup @ setup)

  | Texp_match (e, cases, partial) ->
   let cases = List.map (fun case -> match split_pattern case.c_lhs with
      | (Some p, None) -> {case with c_lhs=p}
      | _ -> raise NotSupported (* No exception patterns allowed *)) cases in
   let (arg, arg_setup) = translate_imm e in
   let matrix = List.map case_to_row cases in
   (* TODO: Include partial/total information? *)
  let (body, setup) = compile_matrix fail_trap [arg] matrix in
  (body, arg_setup @ setup)

  | Texp_letop {let_; ands; param; body; partial} -> translate_letop let_ ands param body partial
  | _ -> raise NotSupported

and case_to_row {c_lhs; c_guard; c_rhs} =
  ([c_lhs], (translate_compound c_rhs, []), Option.map translate_compound c_guard)

and translate_linast ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) =
  let (compound, setup) = translate_compound e in binds_to_anf setup (LinastExpr.compound compound)

(* AND/OR handled specially as they sometimes don't evaluate their second argument *)
and translate_prim_app (primDesc : Primitive.description) args =
    (* Want to represent arrays the same way as tuples, so they get their own get/set operation rather than unary/binary *)
    match (primDesc.prim_name, args) with
      | "%array_safe_get", [arg1; arg2] -> let (imm1, setup1) = translate_imm arg1 in
        let (imm2, setup2) = translate_imm arg2 in (Compound.arrayget imm1 imm2, setup1 @ setup2)
      | "%array_safe_set", [arg1; arg2; arg3] ->
        let (imm1, setup1) = translate_imm arg1 in
        let (imm2, setup2) = translate_imm arg2 in
        let (imm3, setup3) = translate_imm arg3 in
        (Compound.arrayset imm1 imm2 imm3, setup1 @ setup2 @ setup3)
      | _, _ -> (match (Hashtbl.find Primitives.prim_table primDesc.prim_name, args) with
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
      )

and translate_function param cases partial =
  (* TODO: Use this first one or not? Current Wasm implementation doesn't allow currying! *)
   (* match cases with [{c_lhs=pat; c_guard=None;
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
  | _ -> *)
   let matrix = List.map case_to_row cases in
   let (body, setup) = compile_matrix fail_trap [Imm.id param] matrix in
   (Compound.mkfun [param] (binds_to_anf setup (LinastExpr.compound body)), [])


(* Taken from transl_core.transl_letop of OCaml compiler *)
(*
Loop: f [andop{op, e}] -> op e f
f [and1; and2; ...; andn] -> op1 e1 (op2 e2 (... (opn en f)))

transl_letop letop ands id body =
op = letop_id
e = loop letop_e ands = and1 a1 (... (andn an let_e))
f = id -> body

app letop_id e f = letop_id (and1 a1 (... (andn an let_e))) (fun id -> body)
*)
and translate_letop letop ands param case partial =
  let rec loop (prev_body, prev_setup) = function
    | [] -> (prev_body, prev_setup)
    | andop :: rest ->
        let left_id = Ident.create_local "left" in
        let right_id = Ident.create_local "right" in
        let op = translate_ident andop.bop_op_path andop.bop_op_val.val_kind
        in
        let (exp, exp_setup) = translate_compound andop.bop_exp in
        let (lam, lam_setup) =
        (* Bind just does let id = e in body -- unless id = e in which case just returns body *)
        (Compound.app op [Imm.id left_id; Imm.id right_id], exp_setup @ [BLet(right_id, exp)])
      (*    bind right_id exp (Compound.app op [Imm.id left_id; Imm.id right_id])  *)
        in
        let (res, res_setup) = loop (lam, lam_setup) rest
        in (res, prev_setup @ (BLet(left_id, prev_body)) :: res_setup)
      (*  bind left_id prev_lam (loop lam rest)  *)
  in
  let op = translate_ident letop.bop_op_path letop.bop_op_val.val_kind
  in
  let (exp, exp_setup) = loop (translate_compound letop.bop_exp) ands in
  let (func, func_setup) =
    translate_function param [case] partial
  in
  let exp_id = Ident.create_local "andbody" in
  let func_id = Ident.create_local "func" in
  (Compound.app op [Imm.id exp_id; Imm.id func_id], exp_setup @ (BLet(exp_id, exp)) :: func_setup @ [BLet(func_id, func)])


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
 |  [] -> LinastExpr.compound (Compound.imm unit_value)
 | item::items -> (match item.str_desc with
   | Tstr_eval (e, _) -> let (compound, setup) = translate_compound e in
     binds_to_anf setup (LinastExpr.seq compound (translate_structure exported items))
   | Tstr_value (Recursive, bind_list) ->
    let binds, setups = List.fold_right (fun {vb_pat; vb_expr;} (binds, setups) ->
      let id = get_id_from_pattern vb_pat in
      let (compound, setup) = translate_compound vb_expr in
      ((id, (if (List.exists (fun x -> x = id) exported) then Global else Local), compound)::binds, setup@setups)
    ) bind_list ([], [])
    in binds_to_anf setups (LinastExpr.mkletrec binds (translate_structure exported items))
   | Tstr_value (Nonrecursive, []) -> translate_structure exported items
   (* TODO: Should have 'pre-anf' (see Grain) to simplify what can appear in this type of let binding *)
   | Tstr_value (Nonrecursive, {vb_pat;vb_expr;}::bind_list) ->
     let rest = translate_structure exported ({item with str_desc=Tstr_value(Nonrecursive, bind_list)}::items) in
     let matrix = [([vb_pat], rest)] in
     let (value, value_setup) = translate_imm vb_expr in
     let tree = CompileLet.compile_matrix ~exported fail_trap [value] matrix in
     binds_to_anf ~exported value_setup tree

   | _ -> translate_structure exported items (* TODO: Should check which ones should/shouldn't be included *)
  )

let translate_structure_with_coercions (structure, coercions) =
 binds_to_anf (!primBinds) (translate_structure (getExports (structure.str_items, coercions)) structure.str_items)
 (* Add on primitive definitions *)