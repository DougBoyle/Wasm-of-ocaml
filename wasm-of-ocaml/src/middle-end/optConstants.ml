(* CSE only replaces Idents with other Idents, this can replace Idents with Constants.
   i.e. let x = 5 in let y = x in ... => let x = 5 in let y = 5 in ...
   If x was only being used to assign y, will now be removed as a dead assignment *)
open Linast
open Asttypes

let constant_idents = (Ident.Tbl.create 50 : Asttypes.constant Ident.Tbl.t)

(* Ideally want this to happen after body evaluated, just means extra passes needed *)
let enter_linast linast = (match linast.desc with
  | LLet(id, _, body, rest) -> (match body.desc with
    (* Only immediate constants, binop(im1, im2) first reduced by constant folding *)
    | CImm {desc=ImmConst c} -> Ident.Tbl.add constant_idents id c
    | _ -> ())
  | _ -> () (* LLetRec is always a function so no constants to optimise *)
  ); linast

let map_imm (imm : imm_expr) = match imm.desc with
  | ImmIdent id ->
    (match Ident.Tbl.find_opt constant_idents id with Some c -> {imm with desc=ImmConst c} | None -> imm)
  | _ -> imm

(* TODO: Update eval_unary/binary if other types ever need supporting for the same ops *)
let eval_unary = function
  | (Not, Const_int i) -> Const_int (if i > 0 then 0 else 1) (* 0=false, 1=true *)
  (* integer *)
  | (UnNeg, Const_int i) -> Const_int (-i)
  | (UnAdd, c) -> c (* Could actually do this optimisation any time its seen. Doesn't do anything *)
  | (Succ, Const_int i) -> Const_int (succ i)
  | (Pred, Const_int i) -> Const_int (pred i)
  | (Abs, Const_int i) -> Const_int (abs i)
  (* float *)
  | (FUnNeg, Const_float fstr) -> Const_float(Float.to_string(-. (Float.of_string fstr)))
  | (FSqrt, Const_float fstr) -> Const_float(Float.to_string(sqrt (Float.of_string fstr)))
  | _ -> failwith "Type mismatch between constant and operator"

let eval_binary = function
  (* Type checking should have ensured we never see 1 int 1 float *)
  (* floats have to be handled separately (except for testing physical equality),
     since they should be compared by value, not by their strings (representation in Asttypes) e.g. 2.0 = 2.00.
     Writing equality tests of floats in real programs is a bad practice.
     May not behave exactly as OCaml in case of NaNs *)
  | (Eq, Const_float str1, Const_float str2) -> Const_int (if (Float.of_string str1) = (Float.of_string str2) then 1 else 0)
  | (Neq, Const_float str1, Const_float str2) -> Const_int (if (Float.of_string str1) <> (Float.of_string str2) then 1 else 0)
  | (LT, Const_float str1, Const_float str2) -> Const_int (if (Float.of_string str1) < (Float.of_string str2) then 1 else 0)
  | (GT, Const_float str1, Const_float str2) -> Const_int (if (Float.of_string str1) > (Float.of_string str2) then 1 else 0)
  | (LTE, Const_float str1, Const_float str2) -> Const_int (if (Float.of_string str1) <= (Float.of_string str2) then 1 else 0)
  | (GTE, Const_float str1, Const_float str2) -> Const_int (if (Float.of_string str1) >= (Float.of_string str2) then 1 else 0)
  | (Compare, Const_float str1, Const_float str2) -> Const_int (compare (Float.of_string str1) (Float.of_string str2))
  | (Min, Const_float str1, Const_float str2) -> if (Float.of_string str1) < (Float.of_string str2) then Const_float str1 else Const_float str2
  | (Max, Const_float str1, Const_float str2) -> if (Float.of_string str1) > (Float.of_string str2) then Const_float str1 else Const_float str2

  | (Eq, c1, c2) -> Const_int (if c1 = c2 then 1 else 0)
  | (Neq, c1, c2) -> Const_int (if c1 <> c2 then 1 else 0)
  | (LT, c1, c2) -> Const_int (if c1 < c2 then 1 else 0)
  | (GT, c1, c2) -> Const_int (if c1 > c2 then 1 else 0)
  | (LTE, c1, c2) -> Const_int (if c1 <= c2 then 1 else 0)
  | (GTE, c1, c2) -> Const_int (if c1 >= c2 then 1 else 0)
  | (Compare, c1, c2) -> Const_int (compare c1 c2)
  | (Min, c1, c2) -> min c1 c2
  | (Max, c1, c2) -> max c1 c2

  | (Eq_phys, c1, c2) -> Const_int (if c1 == c2 then 1 else 0)
  | (Neq_phys, c1, c2) -> Const_int (if c1 != c2 then 1 else 0)
  (* boolean *)
  | (AND, Const_int i, Const_int j) -> Const_int (if i > 0 && j > 0 then 1 else 0)
  | (OR, Const_int i, Const_int j) -> Const_int (if i > 0 || j > 0 then 1 else 0)
  (* integer *)
  | (Add, Const_int i, Const_int j) -> Const_int (i + j)
  | (Sub, Const_int i, Const_int j) -> Const_int (i - j)
  | (Mult, Const_int i, Const_int j) -> Const_int (i * j)
  (* Division by zero error is caught by caller and optimisation aborted *)
  | (Div, Const_int i, Const_int j) -> Const_int (i / j)
  | (Mod, Const_int i, Const_int j) -> Const_int (i mod j)
  (* list *)
  | (Append, _, _) -> failwith "Append should never get constants as arguments" (* Update if tag encoding changes *)
  (* float *)
  | (FAdd, Const_float fstr1, Const_float fstr2) ->
    Const_float(Float.to_string((Float.of_string fstr1) +. (Float.of_string fstr2)))
  | (FSub, Const_float fstr1, Const_float fstr2) ->
    Const_float(Float.to_string((Float.of_string fstr1) -. (Float.of_string fstr2)))
  | (FMult, Const_float fstr1, Const_float fstr2) ->
    Const_float(Float.to_string((Float.of_string fstr1) *. (Float.of_string fstr2)))
  | (FDiv, Const_float fstr1, Const_float fstr2) ->
    Const_float(Float.to_string((Float.of_string fstr1) /. (Float.of_string fstr2)))
  | _ -> failwith "Type mismatch between constant and operator"

(* Once had the chance to replace immIdents with constants, see if unary/binary can be optimised too *)
let leave_compound (compound : compound_expr) = match compound.desc with
  | CUnary(op, ({desc=ImmConst c} as imm)) -> {compound with desc=CImm {imm with desc=ImmConst (eval_unary (op, c))}}
  | CBinary(op, ({desc=ImmConst c1} as imm1), ({desc=ImmConst c2})) ->
    (try {compound with desc=CImm {imm1 with desc=ImmConst (eval_binary (op, c1, c2))}}
     with Division_by_zero -> compound) (* Leave runtime division-by-zero error till runtime *)

  (* Do actual changing of tree structure here so that analyse.ml can just be a side-effect function *)
  (* Separates analysis from optimisations *)
  | CGetTag imm -> (match List.find_opt (function Tag _ -> true | _ -> false) (!(imm.annotations)) with
    | Some (Tag i) -> {compound with desc=CImm (LinastUtils.Imm.const (Asttypes.Const_int i))}
    | None -> compound
    | _ -> failwith "Find returned an annotation of the wrong kind")
  | CField (imm, idx) ->
    (* Returns a value, so can't wrap in a List.iter *)
    (match List.find_opt (function FieldImms _ -> true | _ -> false) (!(imm.annotations)) with
      | None -> compound
      | Some (FieldImms l) -> (match List.nth_opt l idx with (* nth_opt to avoid impossible case errors *)
          | Some (Some imm) -> {compound with desc=CImm imm}
          | _ -> compound)
      | _ -> failwith "Filter failed to find just FieldImms")
  (* No use looking at ArrayGet, can't yet make any guarentees about the field *)
  | _ -> compound

(* ------------- Dead branch elimination ------------- *)

let can_simplify_branch : compound_expr -> bool = function
  | {desc=CIf({desc=ImmConst _}, _, _)} -> true
  | {desc=CSwitch({desc=ImmConst _}, _, _)} -> true
  | _ -> false

let simplify_branch : compound_expr -> linast_expr = function
  | {desc=CIf({desc=ImmConst (Asttypes.Const_int i)}, branch1, branch2)} -> if i > 0 then branch1 else branch2
  | {desc=CSwitch({desc=ImmConst (Asttypes.Const_int i)}, cases, default)} ->
    (match List.assoc_opt i cases with
      | Some body -> body
      | None -> match default with Some body -> body
               (* Pattern match failure *)
               | None -> LinastUtils.LinastExpr.compound (LinastUtils.Compound.fail (-1l)))
  | _ -> failwith "Cannot simplify this compound"

(* mkleaf creates terminal linast, allows using same function for seq and let bindings *)
let rec rewrite_tree mkleaf branch rest = match branch.desc with
  | LCompound compound -> mkleaf compound rest
  | LSeq (compound, body) -> LinastUtils.LinastExpr.seq compound (rewrite_tree mkleaf body rest)
  | LLet (id, global, compound, body) ->
    LinastUtils.LinastExpr.mklet id global compound (rewrite_tree mkleaf body rest)
  | LLetRec (binds, body) -> LinastUtils.LinastExpr.mkletrec binds (rewrite_tree mkleaf body rest)

let leave_linast linast = match linast.desc with
  (* Dead branches - good idea to do in same pass as optConstants or not? *)
  (* Branches optimised at linast level as tree may need 're-linearising'. By of if statement is linast *)
  | LCompound compound when can_simplify_branch compound -> simplify_branch compound
  (* May need to 're-linearise' the tree now that we have pulled a Linast_expr out of a compound *)
  | LSeq (compound, rest) when can_simplify_branch compound ->
    let branch = simplify_branch compound in
    let mkleaf = LinastUtils.LinastExpr.seq in
    (match branch.desc with (* Keep annotations on top-level linast, rewrite_sequence generates a new one *)
      | LCompound compound -> {linast with desc=LSeq(compound, rest)}
      | LSeq (compound, body) -> {linast with desc=LSeq(compound, rewrite_tree mkleaf body rest)}
      | LLet (id, global, compound, body) ->
        {linast with desc=LLet(id, global, compound, rewrite_tree mkleaf body rest)}
      | LLetRec (binds, body) -> {linast with desc=LLetRec(binds, rewrite_tree mkleaf body rest)}
    )
  (* Not necessary as ident won't be reused once out of scope, but removing helps keep table small
     TODO: By doing this, can't optimise bindings passed through an exit to a handler
           (not yet done by pattern match).
           Implicitly passing out values is starting to look like an issue, any way to avoid this scope issue? *)
  | LLet (id, global, compound, rest) when can_simplify_branch compound ->
    (match compound.desc with (* remove from table *)
      | CImm {desc=ImmConst c} -> Ident.Tbl.remove constant_idents id
      | _ -> ());
    let branch = simplify_branch compound in
    let mkleaf = LinastUtils.LinastExpr.mklet id global in
    (match branch.desc with (* Keep annotations on top-level linast, rewrite_sequence generates a new one *)
      | LCompound compound -> {linast with desc=LLet (id, global, compound, rest)}
      | LSeq (compound, body) -> {linast with desc=LSeq(compound, rewrite_tree mkleaf body rest)}
      | LLet (id', global', compound', body) ->
        {linast with desc=LLet(id', global', compound', rewrite_tree mkleaf body rest)}
      | LLetRec (binds, body) -> {linast with desc=LLetRec(binds, rewrite_tree mkleaf body rest)}
    )
  | LLet(id, _, body, rest) -> (match body.desc with (* remove from table *)
     | CImm {desc=ImmConst c} -> Ident.Tbl.remove constant_idents id
     | _ -> ()); linast
  | _ -> linast

let optimise linast =
  Ident.Tbl.clear constant_idents;
  (LinastMap.create_mapper ~map_imm ~leave_compound ~enter_linast ~leave_linast ()) linast
