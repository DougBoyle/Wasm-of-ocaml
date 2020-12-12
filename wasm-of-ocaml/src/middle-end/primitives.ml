open Linast
open LinastUtils

type arity = Unary of unop | Binary of binop
(* Implemented at IR level, functional form allows getting as operators, but also inlining when fully applied.
   Either apply function to new idents, for creating the body of a function to pass back an operator,
   or apply to the actual immediates to inline them. *)
  | CompoundUnary of (Linast.imm_expr -> Linast.compound_expr * LinastUtils.linast_setup list)
  | CompoundBinary of (Linast.imm_expr -> Linast.imm_expr -> Linast.compound_expr * LinastUtils.linast_setup list)

let prim_table = Misc.create_hashtable 31 [
    "%equal", Binary(Eq);
    "%notequal", Binary(Neq);
    "%lessequal", Binary(LTE);
    "%lessthan", Binary(LT);
    "%greaterequal", Binary(GTE);
    "%greaterthan", Binary(GT);
    "%compare", Binary(Compare);
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

    "%negfloat", Unary(FUnNeg);
    "%addfloat", Binary(FAdd);
    "%subfloat", Binary(FSub);
    "%mulfloat", Binary(FMult);
    "%divfloat", Binary(FDiv);
    "caml_sqrt_float", Unary(FSqrt);

    (* Reference operations just mapped to other expressions *)
    "%makemutable", CompoundUnary(fun imm -> (Compound.makeblock 0l [imm], []));
    "%field0", CompoundUnary(fun imm -> (Compound.field imm 0l, []));
    "%setfield0", CompoundBinary(fun imm1 imm2 -> (Compound.setfield imm1 0l imm2, []));
    "%incr", CompoundUnary(let v1 = Ident.create_local "old_value" and v2 = Ident.create_local "new_value" in
      fun imm -> (Compound.setfield imm 0l (Imm.id v2),
                 [BLet(v1, Compound.field imm 0l); BLet(v2, Compound.unary Succ (Imm.id v1));]));
    "%decr", CompoundUnary(let v1 = Ident.create_local "old_value" and v2 = Ident.create_local "new_value" in
      fun imm -> (Compound.setfield imm 0l (Imm.id v2),
                 [BLet(v1, Compound.field imm 0l); BLet(v2, Compound.unary Pred (Imm.id v1));]));
]

let make_if_fun args setupid setupval cond compound1 compound2 =
 match args with [] -> failwith "Given empty args"
 | x::xs ->
  Compound.mkfun [x]
  (List.fold_right (fun id expr -> LinastExpr.compound (Compound.mkfun [id] expr)) xs
   (
   LinastExpr.mklet setupid Local setupval
    (LinastExpr.compound
     (Compound.mkif
      cond
      (LinastExpr.compound compound1)
      (LinastExpr.compound compound2)))))

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
   (* Compound.mkfun [l1; l2]  HAVE TO REWRITE UNTIL CURRYING/TUPLES SORTED LOWER DOWN *)
   Compound.mkfun [l1] (LinastExpr.compound (Compound.mkfun [l2]
    (binds_to_anf [BLet(tag, tag_c)]
      (LinastExpr.compound
        (Compound.mkswitch
          tagid (* NOTE: Cases dependent on how constructor tags are encoded *)
          [(0l, LinastExpr.compound (Compound.imm l2id));
           (1l, binds_to_anf [BLet(hd, hd_c); BLet(tl, tl_c); BLet(app, app_c)] (LinastExpr.compound result_block))]
          None))))));
]

let bindBinary opId operator =
    let id1, id2 = Ident.create_local "prim", Ident.create_local "prim" in
    (* TODO: Change to specifying curried/tuple to make this neater (for now just done to make work) *)
    BLet(opId,
    Compound.mkfun [id1] (LinastExpr.compound (Compound.mkfun [id2]
    (LinastExpr.compound (Compound.binary operator (Imm.id id1) (Imm.id id2))))))

(* Compound.mkfun [id1; id2] (LinastExpr.compound (Compound.binary operator (Imm.id id1) (Imm.id id2)))) *)


let bindUnary opId operator =
    let id = Ident.create_local "prim" in
    BLet(opId, Compound.mkfun [id] (LinastExpr.compound (Compound.unary operator (Imm.id id))))
  (*  BLet(opId,
      CFunction([id], LCompound(CUnary(operator, ImmIdent id)))) *)

let make_unary_function c_fun =
  let id = Ident.create_local "arg" in
  let (expr, setup) = c_fun (Imm.id id) in
  Compound.mkfun [id] (binds_to_anf setup (LinastExpr.compound expr))

let make_binary_function c_fun =
  let id1 = Ident.create_local "arg1" and id2 = Ident.create_local "arg2" in
  let (expr, setup) = c_fun (Imm.id id1) (Imm.id id2) in
  Compound.mkfun [id1; id2] (binds_to_anf setup (LinastExpr.compound expr))

(* Creates a function performing that operation, adds it to the list of bindings + primitves mapped,
   and returns the Ident to use. *)
(* Note that in this case, && and || DO evaluate both arguments, so can safely handle in this way rather than an if statement *)
let bindOp name =
  let opId = Ident.create_local name in
  let binding = match Hashtbl.find prim_table name with
    | Unary unop -> bindUnary opId unop
    | Binary binop -> bindBinary opId binop
    | CompoundUnary c -> BLet(opId, make_unary_function c)
    | CompoundBinary c -> BLet(opId, make_binary_function c)
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
