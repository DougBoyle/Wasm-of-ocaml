open Linast
open LinastUtils

type arity = Unary of unop | Binary of binop

let prim_table = Misc.create_hashtable 21 [
    "%equal", Binary(Eq);
    "%notequal", Binary(Neq);
    "%lessequal", Binary(LTE);
    "%lessthan", Binary(LT);
    "%greaterequal", Binary(GTE);
    "%greaterthan", Binary(GT);
    "%compare", Binary(Compare);
    (* TODO: min/max/abs/min_int/max_int/@ *)
    "%eq", Binary (Eq_phys);
    "%noteq", Binary (Neq_phys);

    "%sequand", Binary (AND);
    "%sequor", Binary (OR);
    "%boolnot", Unary (Not);

    "%identity", Unary (UnAdd);
    "%negint", Unary (UnNeg);
    "%succint", Unary (Succ);
    "%predint", Unary (Pred);
    "%addint", Binary (Add);
    "%subint", Binary (Sub);
    "%mulint", Binary (Mult);
    "%divint", Binary (Div);
    "%modint", Binary (Mod);
]

let bindBinary opId operator =
    let id1, id2 = Ident.create_local "prim", Ident.create_local "prim" in
    BLet(opId, Compound.mkfun [id1; id2] (LinastExpr.compound (Compound.binary operator (Imm.id id1) (Imm.id id2))))
  (*    CFunction([id1; id2], LCompound(CBinary(operator, ImmIdent id1, ImmIdent id2))) *)

let bindUnary opId operator =
    let id = Ident.create_local "prim" in
    BLet(opId, Compound.mkfun [id] (LinastExpr.compound (Compound.unary operator (Imm.id id))))
  (*  BLet(opId,
      CFunction([id], LCompound(CUnary(operator, ImmIdent id)))) *)

(* Creates a function performing that operation, adds it to the list of bindings + primitves mapped,
   and returns the Ident to use. *)
(* Note that in this case, && and || DO evaluate both arguments, so can safely handle in this way rather than an if statement *)
let bindOp name =
  let opId = Ident.create_local name in
  let binding = match Hashtbl.find prim_table name with
    | Unary unop -> bindUnary opId unop
    | Binary binop -> bindBinary opId binop
  in
  primBinds := binding :: (!primBinds);
  primIds := (name, opId) :: (!primIds);
  opId

let translate_prim (primDesc : Primitive.description) =
    match List.assoc_opt primDesc.prim_name (!primIds) with
    | Some id -> id
    | None -> bindOp primDesc.prim_name