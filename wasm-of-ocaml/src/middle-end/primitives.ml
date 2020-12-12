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

    (* Not actually primitives, but can be treated as such *)
    "abs", Unary(Abs);
    "min", Binary(Min);
    "max", Binary(Max);
    "@", Binary(Append);
]

let bindBinary opId operator =
    let id1, id2 = Ident.create_local "prim", Ident.create_local "prim" in
    (* TODO: Change to specifying curried/tuple to make this neater (for now just done to make work) *)
    BLet(opId,
    Compound.mkfun [id1; id2]
        (LinastExpr.compound (Compound.binary operator (Imm.id id1) (Imm.id id2))))

let bindUnary opId operator =
    let id = Ident.create_local "prim" in
    BLet(opId, Compound.mkfun [id] (LinastExpr.compound (Compound.unary operator (Imm.id id))))

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
    | Some id -> Imm.id id
    | None -> begin match s with
      | "@" | "abs" | "min" | "max" -> Imm.id (bindOp s)
      | "max_int" -> Imm.const (Const_int (Int32.to_int (Int32.div Int32.min_int 2l)))
      | "min_int" -> Imm.const (Const_int (Int32.to_int (Int32.div Int32.min_int 2l)))
      | _ -> failwith "Unsupported Stdlib primitive"
    end

let translate_prim (primDesc : Primitive.description) =
    match List.assoc_opt primDesc.prim_name (!primIds) with
    | Some id -> id
    | None -> bindOp primDesc.prim_name
