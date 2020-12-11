open Linast
open LinastUtils

type arity = Unary of unop | Binary of binop

let prim_table = Misc.create_hashtable 24 [
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

    (* Not actually primitives, but can be treated as such *)
    "abs", Unary(Abs);
    "min", Binary(Min);
    "max", Binary(Max);
    "@", Binary(Append);
]

let make_if_fun args setupid setupval cond compound1 compound2 =
 match args with [] -> failwith "Given empty args"
 | x::xs ->
 (* Compound.mkfun args  HAVE TO REWRITE UNTIL TUPLES/CURRIED SORTED OUT *)
  Compound.mkfun [x]
  (List.fold_right (fun id expr -> LinastExpr.compound (Compound.mkfun [id] expr)) xs
   (
   LinastExpr.mklet setupid Local setupval
    (LinastExpr.compound
     (Compound.mkif
      cond
      (LinastExpr.compound compound1)
      (LinastExpr.compound compound2)))))


let bindBinary opId operator =
    let id1, id2 = Ident.create_local "prim", Ident.create_local "prim" in
    (* TODO: Change to specifying curried/tuple to make this neater (for now just done to make work) *)
    BLet(opId,
    Compound.mkfun [id1] (LinastExpr.compound (Compound.mkfun [id2]
    (LinastExpr.compound (Compound.binary operator (Imm.id id1) (Imm.id id2))))))


let bindUnary opId operator =
    let id = Ident.create_local "prim" in
    BLet(opId, Compound.mkfun [id] (LinastExpr.compound (Compound.unary operator (Imm.id id))))

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
