
(* To be extended with any annotations needed during optimisation analysis e.g. live variables etc. *)
type annotation

(* Deprecated stdlib operators removed *)
type binop =
  (* comparison *)
  | Eq
  | Neq
  | LT
  | GT
  | LTE
  | GTE
  | Compare
  | Min
  | Max
  | Eq_phys
  | Neq_phys
  (* boolean *)
  | AND
  | OR
  (* integer *)
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  (* list *)
  | Append

type unop =
  (* boolean *)
  | Not
  (* integer *)
  | UnNeg
  | UnAdd (* Identity - part of OCaml due to principle of least suprise, just the identity op *)
  | Succ
  | Pred
  | Abs

(* TODO: Also need constants for max_int and min_int *)

type imm_expr_desc =
  | ImmIdent of Ident.t
  | ImmConst of Asttypes.constant

type imm_expr = {desc : imm_expr_desc; loc : Location.t; env : Env.t; annotations : (annotation list) ref}

type partialFlag = Partial | Total
type recFlag = Recursive | Nonrecursive
type dirFlag = Upto | Downfrom

type compound_expr_desc =
  | CImm of imm_expr
  | CUnary of unop * imm_expr
  | CBinary of binop * imm_expr * imm_expr
  | CSetField of imm_expr * int32 * imm_expr (* Field is just 0 for reference objects *)
  | CField of imm_expr * int32
  | CMakeBlock of int32 * imm_expr list (* Tuples, References, Datatypes (constant/block). Mutable flag needed or not? *)
  | CGetTag of imm_expr
  (* if(i, e_1, e_2) evals e_1 if i=0 else e_2 *)
  | CIf of imm_expr * linast_expr * linast_expr
  | CWhile of imm_expr * linast_expr
  (* for i = e1 direction e2 do linast_expr - variable will be modified by local get/set *)
  | CFor of Ident.t * imm_expr * imm_expr * dirFlag * linast_expr
  (* No stringswitch for now - Wasm doesn't have strings so may not implement any string manipulation until later *)
  (* Will match both constant and block tags. Guards are encoded as part of the enclosed expression *)
  | CSwitch of imm_expr * (int * linast_expr) list * partialFlag
  (* Evaluate body, escape to second expression if constant pattern/guard expression fails. *)
  | CMatchTry of int32 * linast_expr * linast_expr
  (* Guard expression or constant pattern failed, escape to trying other patterns. int32 allows identifying one of
     several nested CMatchTry expressions, can optimise in future to jump past some ruled out cases e.g:
     A(5, _) when e -> ...  Fail on testing 5 can jump to 3rd case, fail on testing e jumps to 2nd case
     A(5, _) -> ...
     ...  *)
  | CMatchFail of int32
  | CApp of imm_expr * imm_expr list
  (* AppBuiltin left out, is part of binop equal, makeblock, etc. These call runtime when translated, not at this level *)
  | CFunction of Ident.t list * linast_expr (* Recursion done at higher level *)
  (* TODO: Leave out until I know if I can support strings in wasm or not: | CString of string *)

and compound_expr = {desc : compound_expr_desc; loc : Location.t; env : Env.t; annotations : (annotation list) ref}

and linast_expr_desc =
  (* TODO: What is global-flag being used for in Grain anf tree? - telling which symbols to export maybe? *)
  | LLet of recFlag * (Ident.t * compound_expr) list * linast_expr
  | LSeq of compound_expr * linast_expr
  | LCompound of compound_expr (* Squence of 'let's is ended by a block of all exported identifiers *)

and linast_expr = {desc : linast_expr_desc; loc : Location.t; env : Env.t; annotations : (annotation list) ref}


(* Both grain and Lambda have a single tree at top, not a list anymore *)
(* How to close off sequence of let expressions? Lambda puts an object containing all toplevel elements at end, don't
   know what grain does - what is the benefit of this, to identify which things should be exported? *)