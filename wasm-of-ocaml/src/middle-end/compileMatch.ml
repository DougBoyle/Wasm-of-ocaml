open LinastUtils
open Typedtree

(* pattern -> comp_expr -> BLet list *)
(* value of expression needed in case that it first needs binding to something e.g. TPat_var or alias *)
(* TODO: Must be able to indicate when match has failed e.g. let 5 = x in ...  Need to use fail -1 *)
(* For 'let pattern = expr ...' statements *)
(* TODO: Need to track when to use fail -1 (trap) vs fail 0 - depends if inside an OR pattern or not! *)
let rec getBindings (pat : pattern) expr = match pat.pat_desc with
  | Tpat_any -> []
  | Tpat_var (x, _) -> [BLet(x, expr)]
  | Tpat_alias (p, x, _) -> (BLet(x, expr)) :: getBindings p expr
  | Tpat_constant c ->
    let id = Ident.create_local "constant" in
    let test_result = Compound.binary Eq (Imm.const c) (Imm.id id) in
    let testid = Ident.create_local "isequal" in
    [BLet(id, expr); BLet(testid, test_result);
    BEffect(Compound.mkif
      (Imm.id testid)
      (LinastExpr.compound (Compound.makeblock 0l []))   (* Test passed - do nothing TODO: Use a global unit value *)
      (LinastExpr.compound (Compound.imm (Imm.fail (-1l)))))] (* Test failed - trap *)
  | Tpat_tuple l ->
    let id = Ident.create_local "tuple" in
    let tuple_imm = Imm.id id in
    let tuple_element_binds index pat =
      let arg_id = Ident.create_local "tuple_arg" in
      let arg_expr = Compound.imm (Imm.id arg_id) in
      let sub_binds = getBindings pat arg_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(arg_id, Compound.field tuple_imm (Int32.of_int index)))::sub_binds)
    in let binds = List.concat (List.mapi tuple_element_binds l) in
    (match binds with [] -> [] | _ -> (BLet(id, expr)::binds))

  | Tpat_construct (_, {cstr_tag}, pl) ->
    let id = Ident.create_local "construct" in
    let block_imm = Imm.id id in

    (* Test tag - fail if not a match *)
    let tag_id = Ident.create_local "construct_tag" in
    let tag_expr = Compound.gettag block_imm in
    (* Note that this puts an int32 constant in the tree, instead of the usual const_int - other integers aren't converted *)
    let test_result = Compound.binary Eq (Imm.const (Const_int32 (unify_constructor_tag cstr_tag))) (Imm.id tag_id) in
    let testid = Ident.create_local "isequal" in

    (* If tag matches - bind subcomponents *)
    let block_element_binds index pat =
      let elem_id = Ident.create_local "element" in
      let elem_expr = Compound.imm (Imm.id elem_id) in
      let sub_binds = getBindings pat elem_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(elem_id, Compound.field block_imm (Int32.of_int index)))::sub_binds)
    in let sub_binds = List.concat (List.mapi block_element_binds pl) in

    [BLet(id, expr);
    BLet(tag_id, tag_expr);
    BLet(testid, test_result);
    BEffect(Compound.mkif
      (Imm.id testid)
      (LinastExpr.compound (Compound.makeblock 0l []))   (* Test passed - do nothing TODO: Use a global unit value *)
      (LinastExpr.compound (Compound.imm (Imm.fail (-1l))))); (* Test failed - trap TODO: Fail to correct level *)
    ]  @ sub_binds

  | Tpat_record (l, _) -> raise NotImplemented
  (* Don't expect to need to match Tpat_value or Tpat_lazy -- GADT, they aren't value patterns *)
  (* Unsure about OR case - how does it interact with matching single or multiple components? *)
  | Tpat_or (p1, p2, _) -> raise NotImplemented
  | Tpat_array pl -> raise NotImplemented
  | Tpat_variant (_, _, _) -> raise NotImplemented
  | _ -> raise NotSupported