open LinastUtils
open Typedtree

(* pattern -> comp_expr -> BLet list *)
(* value of expression needed in case that it first needs binding to something e.g. TPat_var or alias *)
(* TODO: Must be able to indicate when match has failed e.g. let 5 = x in ...  Need to use fail -1 *)
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
      (LinastExpr.compound (Compound.makeblock 0l []))   (* Test passed - do nothing *)
      (LinastExpr.compound (Compound.imm (Imm.fail (-1l)))))] (* Test failed - trap *)
  | Tpat_tuple l -> raise NotImplemented

  | Tpat_construct (id, desc, pl) -> raise NotImplemented
  | Tpat_record (l, _) -> raise NotImplemented
  | Tpat_array pl -> raise NotImplemented
  (* Don't expect to need to match Tpat_value or Tpat_lazy -- GADT, they aren't value patterns *)
  (* Unsure about OR case - how does it interact with matching single or multiple components? *)
  | Tpat_or (p1, p2, _) -> raise NotImplemented
  | Tpat_variant (_, _, _) -> raise NotImplemented
  | _ -> raise NotSupported