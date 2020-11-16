open Typedtree
open Linast
open Types
open CompileMatch
open LinastUtils

(* May end up needing different functions for translating to linast vs compound vs imm expr *)
(* Can encode quite nicely as a value of required type accompanied with a set of needed linast bindings (to do left-to-right) *)

(* A list equivalent to a Linast: Collect a sequence of the bindings needed as we translate things
   then merge them into a Linast tree at the end. *)

let translate_ident path = function
  | Val_prim p -> Primitives.translate_prim p
  (* May need to handle some identifiers which point directly to runtime functions e.g. Stdlib!.min
     Could also just 'hack' these into the file as usual idents but would be inefficient and probably slower. *)
  | _ -> (match path with Path.Pident id -> id | _ -> raise NotImplemented)

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
      let bindList = getBindings vb_pat exp in
      let (rest, rest_setup) = translate_imm ({e with exp_desc=Texp_let(Nonrecursive, rest, body)}) in
      (rest, exp_setup @ bindList @ rest_setup)

  | Texp_let(Recursive, binds, body) ->
      let (binds, binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, translate_compound vb_expr)) binds) in
      let (required_binds, required_binds_setup) = List.split binds_setup in
      let names = List.map (function
          | {pat_desc=Tpat_var(id, _)} -> id
          | _ -> raise NotSupported (* LHS of let rec must be a function identifier *)) binds in
      let (body, body_setup) = translate_compound body in
      let bodyIdent = Ident.create_local "letrec" in
      (Imm.id bodyIdent, (List.concat required_binds_setup)
                             @ [BLetRec (List.combine names required_binds)]
                             @ body_setup
                             @ [BLet(bodyIdent, body)])


  | Texp_tuple l ->
    let id = Ident.create_local "tuple" in
    let (args, setup) = List.split (List.map translate_imm l) in
    (Imm.id id, (List.concat setup) @ [BLet(id, Compound.makeblock 0l args)])

  | Texp_construct (identLoc, desc, l) ->
    let id = Ident.create_local "block" in
    let (args, setup) = List.split (List.map translate_imm l) in
    (Imm.id id, (List.concat setup) @ [BLet(id, Compound.makeblock (unify_constructor_tag desc.cstr_tag) args)])

 (* Made easier by fact that Typedtree always puts fields in order, regardless of order in program.
    Hence no need to do sorting or check label descriptions *)
 (* TODO: Having to convert arrays to lists suggests I should represent things slightly differently here *)
  | Texp_record {fields; extended_expression} ->
    let id = Ident.create_local "record" in
    (match extended_expression with
    (* Not built off of anything so each field must be Overridden *)
    | None -> let (args, setup) =
        List.split (List.map (function (_, Overridden(_, e)) -> translate_imm e | _ -> raise NotSupported) (Array.to_list fields))
      in (Imm.id id, (List.concat setup) @ [BLet(id, Compound.makeblock 0l args)])
    | Some e -> let extract_field original i = (function
         | (_, Overridden(_, e)) -> translate_imm e
         | (_, Kept _) -> let fieldId = Ident.create_local "field" in
           (Imm.id fieldId, [BLet(id, Compound.field original (Int32.of_int i))]))
       in let (original, original_setup) = translate_imm e in
       let (args, setup) = List.split (List.mapi (extract_field original) (Array.to_list fields))
       in (Imm.id id, original_setup @ (List.concat setup) @ [BLet(id, Compound.makeblock 0l args)])
    )

  | Texp_field (e, identLoc, labelDesc) ->
    let id = Ident.create_local "field" in
    let (record, setup) = translate_imm e in
    (Imm.id id, setup @ [BLet(id, Compound.field record (Int32.of_int labelDesc.lbl_pos))])

  | Texp_setfield (e, identLoc, labelDesc, v) ->
    let id = Ident.create_local "setfield" in
    let (record, record_setup) = translate_imm e in
    let (value, value_setup) = translate_imm v in
    (Imm.id id, record_setup @ value_setup @ [BLet(id, Compound.setfield record (Int32.of_int labelDesc.lbl_pos) value)])

  | Texp_ifthenelse (e1, e2, e3opt) ->
    let (e3_lin, e3_setup) = (match e3opt with Some e -> translate_linast e
      (* TODO: Use a single global unit, or treat as int not block *)
      | None -> (LinastExpr.compound (Compound.makeblock 0l []), [])) in
    let (e1_imm, e1_setup) = translate_imm e1 in
    let (e2_lin, e2_setup) = translate_linast e2 in
    let id = Ident.create_local "ifthenelse" in
    (Imm.id id, e1_setup @ e2_setup @ e3_setup @ [BLet(id, Compound.mkif e1_imm e2_lin e3_lin)])

  | Texp_sequence (e1, e2) ->
    let id = Ident.create_local "seq" in
    let (effect, effect_setup) = translate_compound e1 in
    let (result, result_setup) = translate_compound e2 in
    (Imm.id id, effect_setup @ [BEffect effect] @ result_setup @ [BLet(id, result)])

  | Texp_while (e1, e2) ->
      let id = Ident.create_local "seq" in
      let (test, test_setup) = translate_imm e1 in
      let (loop, loop_setup) = translate_linast e2 in
      (Imm.id id, test_setup @ loop_setup @ [BLet(id, Compound.mkwhile test loop)])
  (* Translcore in OCaml doesn't use the second pattern arg, just annotation information *)
  | Texp_for (param, _, start, finish, dir, e) ->
    let id = Ident.create_local "for" in
    let (start, start_setup) = translate_imm start in
    let (finish, finish_setup) = translate_imm finish in
    let (expr, expr_setup) = translate_linast e in
    (Imm.id id, start_setup @ finish_setup @ expr_setup @ [BLet (id, Compound.mkfor param start finish dir expr)])

  (* TODO: Look at how translprim collapses down curried functions. Each Texp_function only has 1 argument *)
  | Texp_function { arg_label; param; cases; partial; } ->
    raise NotImplemented
  | Texp_apply (e, args) -> raise NotImplemented
  | Texp_match (e, cases, partial) -> raise NotImplemented
  | Texp_letop {let_; ands; param; body; partial} -> raise NotImplemented
  | _ -> raise NotSupported

and translate_compound ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) = raise NotImplemented

and translate_linast ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) = raise NotImplemented

let translate_binding {vb_pat; vb_expr; vb_attributes; vb_loc} = raise NotImplemented

let translate_structure_item {str_desc; str_loc; str_env} =
  match str_desc with
  | Tstr_eval (e, _) -> raise NotImplemented
  | Tstr_value (Nonrecursive, bindList) -> raise NotImplemented
  | Tstr_value (Recursive, bindList) -> raise NotImplemented
  | Tstr_type _ | Tstr_typext _ | Tstr_attribute _ -> raise NotImplemented (* Should be ignored *)
  | _ -> raise NotSupported

let rec translate_structure {str_items; str_type; str_final_env} = raise NotImplemented