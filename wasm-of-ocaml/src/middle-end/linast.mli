open Asttypes

(* TODO: Add 32/64 bit ints or no? i.e. 0l and 0L operators etc. *)

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

(* To be extended with any annotations needed during optimisation analysis e.g. live variables etc. *)
(* Want to mark which parts of an expression are pure so can be replaced by CSE *)
(* Fields: Copy annotations of fields onto block/imm assigned to *)
(* Pure => evaluation has no side effects. Immutable => Result also can not have been changed by other effects
   Some redundancy here? Think of Pure = dead assignment elimination, Immutable = CSE *)
(* TODO: Impossible annotation to remove cases, and a Tag/Constant annotation for constant propagation
         of blocks.
         Constant annotation used similarly to Fields where value assigned is a known constant.
         Between them, Fields and Constant (base case) allow full constant propagation, don't need
         a special optConstants program except to actually do the replacements. *)
type annotation = Pure (* whole term/body pure *) | Fields of annotations list
  | ImmutableBlock of bool list (* Whether SetField possible for each field - very basic analysis *)
  | Immutable
  (* Assertion: Since tree is linearised and nothing assumed about function args, imms only occur where in scope *)
  | FieldImms of (imm_expr option) list
  | Tag of int (* same as FieldImms but for tracking the tag. No option since annotation removed if None *)
  (* Mark a term as always throwing a particular CFail i *)
  | Fail of int32
  (* TailRecursive -> LFunction can be optimised. TailCall -> this call should be rewritten as tail call *)
  | TailRecursive | TailCall
  | SingleTailRecursive (* Should be made tail recursive on its own, so stays as one function *)
  (* Indicates that a function has been made tail recursive, so can rewrite its tail calls.
     Kept when other annotations cleared. Also prevents trying to tail call optimise it a second time *)
  | TailCallOptimised
  (* Arguments to function are all passed together rather than currying *)
  | Tupled
(* TODO: Can do much more complex analysis for whether mutable fields can be copied or not based on if anything
         could have actually assigned to them or not
   TODO: Analysis pass to propagate annotations through use of idents i.e. Graph based CSE *)
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
  | CSetField of imm_expr * int * imm_expr (* Field is just 0 for reference objects *)
  | CField of imm_expr * int
  | CArraySet of imm_expr * imm_expr * imm_expr
  | CArrayGet of imm_expr * imm_expr
  | CMakeBlock of int * imm_expr list (* Tuples, References, Datatypes (constant/block). Mutable flag needed or not? *)
  | CGetTag of imm_expr
  (* if(i, e_1, e_2) evals e_1 if i=0 else e_2 in Wasm *)
  (* In OCaml, true/false are constant blocks 1/0 respectively, mapped to 2/0 by my program, so need to be careful when
     translating down to Wasm later. *)
  | CIf of imm_expr * linast_expr * linast_expr
  | CWhile of linast_expr * linast_expr
  (* for i = e1 direction e2 do linast_expr - variable will be modified by local get/set *)
  | CFor of Ident.t * imm_expr * imm_expr * direction_flag * linast_expr
  (* No stringswitch for now - Wasm doesn't have strings so may not implement any string manipulation until later *)
  (* Will match both constant and block tags. Guards are encoded as part of the enclosed expression *)
  | CSwitch of imm_expr * (int * linast_expr) list * (linast_expr option)
  (* Evaluate body, escape to second expression if constant pattern/guard expression fails. *)
  (* int32 marker may or may not be needed, either to track nesting depth (need extra 'depth' arg in many functions)
     or just to uniquely identify where to fail to in ImmMatchFail. Giving multiple jump options can speed up matches. *)
  | CMatchTry of int32 * linast_expr * linast_expr
  | CApp of imm_expr * imm_expr list
  (* AppBuiltin left out, is part of binop equal, makeblock, etc. These call runtime when translated, not at this level *)
  | CFunction of Ident.t list * linast_expr (* Recursion done at higher level *)
  (* Mutable idents introduced for tracking parameters when tail-call optimisation performed *)
  (* Separate instruction needed since 'LLet ...' creates a new local every time *)
  | CAssign of Ident.t * imm_expr
  (* TODO: Leave out until I know if I can support strings in wasm or not: | CString of string *)

and compound_expr = {desc : compound_expr_desc; loc : Location.t; env : Env.t; mutable annotations : annotations}

and linast_expr_desc =
  (* global rules may get more challenging if using mli file or considering redeclarations of variables *)
  (* moved global flag - can have some parts of a set of mutually recursive functions being visible without all visible *)
  | LLet of Ident.t * globalFlag * compound_expr * linast_expr
  | LLetRec of (Ident.t * globalFlag * compound_expr) list * linast_expr
  | LSeq of compound_expr * linast_expr
  | LCompound of compound_expr

and linast_expr = {desc : linast_expr_desc; loc : Location.t; env : Env.t; mutable annotations : annotations}
