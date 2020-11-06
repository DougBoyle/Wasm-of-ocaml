
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

