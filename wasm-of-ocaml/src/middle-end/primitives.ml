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

let make_if_fun args setupid setupval cond compound1 compound2 =
  Compound.mkfun args
   (
   LinastExpr.mklet setupid Local setupval
    (LinastExpr.compound
     (Compound.mkif
      cond
      (LinastExpr.compound compound1)
      (LinastExpr.compound compound2))))

let app_ident = Ident.create_local "@"

(* See stdlib.ml for functions being implemented below *)
(* TODO: Should actually just compile those to Wasm after all and change this to just return Idents pointing to runtime *)
let other_prims = Misc.create_hashtable 4 [
  (* Explicitly 32 bits currently *)
  (* Inefficiency of compounds for constants bound to identifiers will be handled by constant propagation *)
  "min_int", Compound.imm (Imm.const (Const_int (Int32.to_int (Int32.div Int32.min_int 2l))));
  "max_int", Compound.imm (Imm.const (Const_int (Int32.to_int (Int32.div Int32.max_int 2l))));
  "abs", (let id = Ident.create_local "param" in
    let test_id = Ident.create_local "test" in
    let test = Compound.binary GTE (Imm.id id) (Imm.const (Const_int 0)) in
    make_if_fun [id] test_id test (Imm.id test_id) (Compound.imm (Imm.id id)) (Compound.unary UnNeg (Imm.id id)));
  "min", (let id1 = Ident.create_local "param1" in
    let id2 = Ident.create_local "param2" in
    let test_id = Ident.create_local "test" in
    let test = Compound.binary LTE (Imm.id id1) (Imm.id id2) in
    make_if_fun [id1; id2] test_id test (Imm.id test_id) (Compound.imm (Imm.id id1)) (Compound.imm (Imm.id id2)));
  "max", (let id1 = Ident.create_local "param1" in
    let id2 = Ident.create_local "param2" in
    let test_id = Ident.create_local "test" in
    let test = Compound.binary GTE (Imm.id id1) (Imm.id id2) in
    make_if_fun [id1; id2] test_id test (Imm.id test_id) (Compound.imm (Imm.id id1)) (Compound.imm (Imm.id id2)));
  "@", (let l1, l2, tag, hd, tl, app = Ident.create_local "l1", Ident.create_local "l2", Ident.create_local "tag",
        Ident.create_local "hd", Ident.create_local "tl", Ident.create_local "app" in
    let l1id, l2id, tagid, hdid, tlid, appid = Imm.id l1, Imm.id l2, Imm.id tag, Imm.id hd, Imm.id tl, Imm.id app in
    let (tag_c, hd_c, tl_c) = Compound.gettag l1id, Compound.field l1id 0l, Compound.field l1id 1l in
    let app_c = Compound.app (Imm.id app_ident) [tlid; l2id] in
    let result_block = Compound.makeblock 1l [hdid; appid] in
    Compound.mkfun [l1; l2]
    (binds_to_anf [BLet(tag, tag_c)]
      (LinastExpr.compound
        (Compound.mkswitch
          tagid
          [(0l, LinastExpr.compound (Compound.imm l2id));
           (1l, binds_to_anf [BLet(hd, hd_c); BLet(tl, tl_c); BLet(app, app_c)] (LinastExpr.compound result_block))]
          None))));
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

let translate_other_prim s =
  match List.assoc_opt s (!primIds) with
    | Some id -> id
    | None -> begin match s with
      | "@" -> let compound = Hashtbl.find other_prims s in
        primBinds := (BLetRec[(app_ident, compound)])::(!primBinds);
        primIds := (s, app_ident)::(!primIds);
        app_ident (* TODO: Is it a good idea to be fixing this global ident, should at least declare in 1 place at top *)
      | _ -> let compound = Hashtbl.find other_prims s in
        let id = Ident.create_local s in
        primBinds := (BLet(id, compound))::(!primBinds);
        primIds := (s, id)::(!primIds);
        id
    end

let translate_prim (primDesc : Primitive.description) =
    match List.assoc_opt primDesc.prim_name (!primIds) with
    | Some id -> id
    | None -> bindOp primDesc.prim_name
