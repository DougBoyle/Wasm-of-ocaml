(* CSE only replaces Idents with other Idents, this can replace Idents with Constants.
   i.e. let x = 5 in let y = x in ... => let x = 5 in let y = 5 in ...
   If x was only being used to assign y, will now be removed as a dead assignment *)
open Linast
open Asttypes

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
  (* [] = 0, so this only happens if both lists 0, in which case can just return 0 too *)
  | (Append, _, _) -> Const_int 0
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
let leave_compound compound = match compound.c_desc with
  | CUnary(op, ({i_desc=ImmConst c} as imm)) ->
    {compound with c_desc=CImm {imm with i_desc=ImmConst (eval_unary (op, c))}}
  | CBinary(op, ({i_desc=ImmConst c1} as imm1), ({i_desc=ImmConst c2})) ->
    (try {compound with c_desc=CImm {imm1 with i_desc=ImmConst (eval_binary (op, c1, c2))}}
     with Division_by_zero -> compound) (* Leave runtime division-by-zero error till runtime *)

  (* Do actual changing of tree structure here so that analyse.ml can just be a side-effect function *)
  (* Separates analysis from optimisations *)
  | CGetTag imm -> (match List.find_opt (function Tag _ -> true | _ -> false) (!(imm.i_annotations)) with
    | Some (Tag i) -> {compound with c_desc=CImm (LinastUtils.Imm.const (Asttypes.Const_int i))}
    | None -> compound
    | _ -> failwith "Find returned an annotation of the wrong kind")
  | CField (imm, idx) ->
    (* Returns a value, so can't wrap in a List.iter *)
    (match List.find_opt (function FieldImms _ -> true | _ -> false) (!(imm.i_annotations)) with
      | None -> compound
      | Some (FieldImms l) -> (match List.nth_opt l idx with (* nth_opt to avoid impossible case errors *)
          | Some (Some imm) -> {compound with c_desc=CImm imm}
          | _ -> compound)
      | _ -> failwith "Filter failed to find just FieldImms")
  (* No use looking at ArrayGet, can't yet make any guarentees about the field *)
  | _ -> compound

(* ------------- Dead branch elimination ------------- *)
(* Also removes switches with only 1 case *)

let can_simplify_branch = function
  | {c_desc=CIf({i_desc=ImmConst _}, _, _)}
  | {c_desc=CSwitch({i_desc=ImmConst _}, _, _)}
  (* Switch with only 1 case and no default. Only created if known to be exhaustive (hence no default)
     so can safely remove the switch *)
  | {c_desc=CSwitch(_, [_], None)} -> true
  | _ -> false

let simplify_branch = function
  | {c_desc=CIf({i_desc=ImmConst (Asttypes.Const_int i)}, branch1, branch2)} -> if i > 0 then branch1 else branch2
  | {c_desc=CSwitch(_, [(_, body)], None)} -> body
  | {c_desc=CSwitch({i_desc=ImmConst (Asttypes.Const_int i)}, cases, default)} ->
    (match List.assoc_opt i cases with
      | Some body -> body
      | None ->
        match default with
          | Some body -> body
          (* pattern match failure - shouldn't occur since defaults are used whenever non-exhaustive *)
          | None -> failwith "Incomplete switch statement (expected a default case)")
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
    let branch = simplify_branch compound in
    let mkleaf = LinastUtils.LinastExpr.mklet id global in
    (match branch.desc with (* Keep annotations on top-level linast, rewrite_sequence generates a new one *)
      | LCompound compound -> {linast with desc=LLet (id, global, compound, rest)}
      | LSeq (compound, body) -> {linast with desc=LSeq(compound, rewrite_tree mkleaf body rest)}
      | LLet (id', global', compound', body) ->
        {linast with desc=LLet(id', global', compound', rewrite_tree mkleaf body rest)}
      | LLetRec (binds, body) -> {linast with desc=LLetRec(binds, rewrite_tree mkleaf body rest)}
    )
  | _ -> linast

let optimise linast =
  (LinastMap.create_mapper ~leave_compound ~leave_linast ()) linast
