open Linast
(* More challenging going to the Grain format - Grain uses datatypes of operations whereas Lambda
   uses a set of Prims with Lists of Lambda args, much more flexible and can pattern match it all *)

(* Can just make more specialised, simply case split for unary vs binary operators *)
type arity = Unary of unop | Binary of binop

let prim_table = create_hashtable 21 [
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

let translate_prim path primDesc =
    let prim = lookup_primitive_and_mark_used (to_location loc) p env path in
    let has_constant_constructor = false in
    let prim =
      match specialize_primitive env ty ~has_constant_constructor prim with
      | None -> prim
      | Some prim -> prim
    in
    let rec make_params n =
      if n <= 0 then []
      else (Ident.create_local "prim", Pgenval) :: make_params (n-1)
    in
    let params = make_params p.prim_arity in
    let args = List.map (fun (id, _) -> Lvar id) params in
    let body = lambda_of_prim p.prim_name prim loc args None in
    match params with
    | [] -> body
    | _ ->
        Lfunction{ kind = Curried;
                   params;
                   return = Pgenval;
                   attr = default_stub_attribute;
                   loc;
                   body; }