open Asttypes

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
  (* float *)
  | FAdd
  | FSub
  | FMult
  | FDiv

type unop =
  (* boolean *)
  | Not
  (* integer *)
  | UnNeg
  | UnAdd (* Identity - part of OCaml due to principle of least suprise, just the identity op. FUnAdd also maps to this *)
  | Succ
  | Pred
  | Abs
  (* float *)
  | FUnNeg
  | FSqrt (* Available directly in Webassembly *)

type imm_expr_desc =
  | ImmIdent of Ident.t
  | ImmConst of Asttypes.constant

(* To be extended with any annotations needed during optimisation analysis *)
type annotation =
  | Pure (* Safe for dead assignment elimination to remove *)
  | Immutable (* Implies expression is also pure, can be optimised by CSE (i.e. no need to recompute) *)
  | Latent of annotations (* analysis of latent-effects of function *)
  | Fields of annotations list (* Allows side-effects analysis to propagate through immutable memory *)

  | ImmutableBlock of bool list (* Whether each field of block immutable *)
  (* Assertion: Since tree is linearised and nothing assumed about function args, imms only occur where in scope *)
  | FieldImms of (imm_expr option) list (* Known immediates in immutable memory *)
  | Tag of int (* Known tag of block *)

  (* Mark a term as always throwing a particular CFail i *)
  | Fail of int32

  (* TailRecursive -> LFunction can be optimised. TailCall -> CApp should be rewritten as tail call *)
  | TailRecursive | TailCall
  (* Made tail recursive on its own, so stays as one function *)
  | SingleTailRecursive
  (* Indicates that a function has been made tail recursive, so can rewrite its tail calls.
     Kept when other annotations cleared. Also prevents trying to tail call optimise it a second time *)
  | TailCallOptimised

  (* Arguments to function are all passed together rather than currying *)
  | Tupled

and annotations = (annotation list) ref

and imm_expr = {i_desc : imm_expr_desc; i_loc : Location.t; i_env : Env.t; mutable i_annotations : annotations}

type partialFlag = Typedtree.partial = Partial | Total

(* To export or not in Wasm.
   Mut indicates mutable local created for tail call optimisations. Must not constant propagate etc.
   (should never actually see Mut on a letRec) *)
type globalFlag = Export | Local | Mut

type compound_expr_desc =
  | CImm of imm_expr
   (* Compound term as it never appears inside another compound and excluding it from immediates
      means all immediates are 'pure' expressions i.e. Idents or Constants
    CMatchFail -1l => top level failure, trap. Other values are jumps to corresponding handler *)
  | CMatchFail of int32
  | CUnary of unop * imm_expr
  | CBinary of binop * imm_expr * imm_expr
  | CSetField of imm_expr * int * imm_expr (* Index = 0 for reference objects *)
  | CField of imm_expr * int
  | CArraySet of imm_expr * imm_expr * imm_expr
  | CArrayGet of imm_expr * imm_expr
  | CMakeBlock of int * imm_expr list (* Tuples, References, Datatypes (constant/block) *)
  | CGetTag of imm_expr
  (* if(i, e1, e2) is 'if i <> 0 then e1 else e2' *)
  | CIf of imm_expr * linast_expr * linast_expr
  | CWhile of linast_expr * linast_expr
  (* for i = e1 direction e2 do linast_expr *)
  | CFor of Ident.t * imm_expr * imm_expr * direction_flag * linast_expr
  (* Will match both constant and block tags. Option is for a default case *)
  | CSwitch of imm_expr * (int * linast_expr) list * (linast_expr option)
  (* Evaluate body, escape to second expression if pattern/guard expression fails. *)
  (* int32 identifies where to fail to in CMatchFail. Giving multiple jump options can speed up matches. *)
  | CMatchTry of int32 * linast_expr * linast_expr
  | CApp of imm_expr * imm_expr list
  | CFunction of Ident.t list * linast_expr
  (* Mutable idents introduced for tracking parameters when tail-call optimisation performed *)
  (* Separate instruction needed since 'LLet ...' creates a new local every time *)
  | CAssign of Ident.t * imm_expr
  (* TODO: Add support for strings - | CString of string *)

and compound_expr = {c_desc : compound_expr_desc; c_loc : Location.t; c_env : Env.t;
  mutable c_annotations : annotations}

and linast_expr_desc =
  | LLet of Ident.t * globalFlag * compound_expr * linast_expr
  (* can have some parts of a set of mutually recursive functions being visible without all visible *)
  | LLetRec of (Ident.t * globalFlag * compound_expr) list * linast_expr
  | LSeq of compound_expr * linast_expr
  | LCompound of compound_expr

and linast_expr = {desc : linast_expr_desc; loc : Location.t; env : Env.t; mutable annotations : annotations}
