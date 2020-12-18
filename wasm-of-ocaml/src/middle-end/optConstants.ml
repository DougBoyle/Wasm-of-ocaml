(* CSE only replaces Idents with other Idents, this can replace Idents with Constants.
   i.e. let x = 5 in let y = x in ... => let x = 5 in let y = 5 in ...
   If x was only being used to assign y, will now be removed as a dead assignment
   TODO: Constant folding i.e. simplifying unops/binops etc. Complex analysis could optimise GetTag too. *)
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
  (* The comparison operations can also take floats.
     Type checking should have ensured we never see 1 int 1 float, so comparisons defined for whole constant *)
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
  (* | CGetTag imm -> compound *) (* TODO: Add analysis for this too, change FieldImms to take a tag option? *)
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

(* Not necessary as ident won't be reused once out of scope, but helps keep table small
   TODO: By doing this, can't optimise bindings passed through an exit to a handler
         (not yet done by pattern match).
         Implicitly passing out values is starting to look like an issue, any way to avoid this scope issue? *)
let leave_linast linast = (match linast.desc with
  | LLet(id, _, body, rest) -> (match body.desc with
    | CImm {desc=ImmConst c} -> Ident.Tbl.remove constant_idents id
    | _ -> ())
  | _ -> ()
  ); linast

(* Allows replacing getField with whatever the field was originally declared as whenever that is known.
  Assertion: Since tree is linearised and nothing assumed about function args, imms only occur where in scope.
  Specialised analysis pass rather than doing along with propagateAnalysis, some work repeated.
  Attempt to do proper merging of annotations for cases/if statements. i.e. If two fields are equal,
  keep that information even if other fields are not. *)



let optimise linast =
  Ident.Tbl.clear constant_idents;
  (LinastMap.create_mapper ~map_imm ~leave_compound ~enter_linast ~leave_linast ()) linast
