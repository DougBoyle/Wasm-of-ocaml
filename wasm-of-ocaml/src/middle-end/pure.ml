(* Test if assignments can be safely removed. *)
(* Neater but less efficient to do as an analysis pass? *)
(* Ideally, if impure then optDeadAssignments would turn it into a sequence rather than a binding *)
(* TODO: Using analysis attached to structures, could likely do smarter analysis i.e. add Pure/Impure flags
         Find an example where this would be beneficial.
         Probably want to rename these functions somehow *)
open Linast
open LinastUtils

(* Test to see if a compound value has any side effects *)
(* Jumps is a list of handlers contained in the expression being evaluated, can ignore them as long
   as that handler is also pure. Assume no pointless handlers present - would wrongly check handler *)
let rec is_comp_pure jumps (comp : Linast.compound_expr) = match comp.desc with
  (* Note - depends on set of expressions implemented using Unary/Binary. Currently all pure *)
  | CImm _ | CUnary _ | CBinary _ | CField _ | CArrayGet _
  | CMakeBlock _ | CGetTag _ | CFunction _ -> true
  | CSetField _ | CArraySet _ -> false
  | CMatchFail i -> List.mem i jumps  (* causes a jump *)
  | CIf (_, thenbody, elsebody) -> is_linast_pure jumps thenbody && is_linast_pure jumps elsebody
  (* Don't expect while statements to be pure, since then they're either useless or loop forever *)
  | CWhile (cond, body) -> is_linast_pure jumps cond && is_linast_pure jumps body
  | CFor (_, _, _, _, body) -> is_linast_pure jumps body
  | CSwitch (imm, cases, default) -> List.for_all (fun (_, body) -> is_linast_pure jumps body) cases &&
    (match default with None -> true | Some body -> is_linast_pure jumps body)
  | CMatchTry (i, body, handle) ->
    is_linast_pure (i :: jumps) body && is_linast_pure jumps handle
  (* TODO: More complex analysis needed to track pure functions, suggests analysis pass required
           to tag idents bound to pure imms. May then need to add imm analysis function to this file *)
  | CApp (imm, args) -> false (* No way to tell what the function does, need analysis pass before this *)

(* When body of compound contains a linast i.e. If(cond, body, else) - want to check if linast has any side-effects *)
and is_linast_pure jumps linast = match linast.desc with
  | LLet (_, _, body, rest) -> is_comp_pure jumps body && is_linast_pure jumps rest
  (* Currently can't analyse functions, so just look at rest since can't do anything with function definitions *)
  | LLetRec (_, rest) -> is_linast_pure jumps rest
  | LSeq (comp, lin) -> is_comp_pure jumps comp && is_linast_pure jumps lin
  | LCompound comp -> is_comp_pure jumps comp

(* Separate analysis needed for CSE to check that value cannot change if repeated.
   TODO: Work out with more intelligent optimisation pass, tracking valid/invalidated common expressions.
         e.g. Need to propagate annotations from 'Let id = comp' to 'MakeBlock(..., Imm.id id, ...)'
   e.g. getField can always be removed as a dead assignment, but can only be optimised in CSE if value
        known to not change between first and last use. Simple analysis would be mut -> False, immutable -> True
        but can be more intelligent and analyse BASIC BLOCKS e.g. getField; getField; setField is fine. *)

(* mostly a subset of the ones true above.
   Need to check that a repeated compound can be replaced with the Ident it was assigned to the first time.
   i.e. term has no visible side effects and result can't have changed since last evaluation.
   Expressions like MakeBlock can still be pure if they return an immutable value, just reuse it. *)
(* TODO: Make more aggressive *)
let rec pure_comp_result (comp : Linast.compound_expr) = match comp.desc with
  | CImm _ | CUnary _ | CBinary _ | CGetTag _ | CFunction _ -> true
  (* SetField could be marked as reusable if we can determine field was already that value *)
  | CSetField _ | CArraySet _ -> false
  | CMatchFail _ -> true (* Can never be reached twice anyway *)
  | CIf (_, thenbody, elsebody) -> pure_lianst_result thenbody && pure_lianst_result elsebody
  (* Don't expect while statements to be pure, since then they're either useless or loop forever *)
  | CWhile (cond, body) -> pure_lianst_result cond && pure_lianst_result body
  | CFor (_, _, _, _, body) -> pure_lianst_result body
  | CSwitch (imm, cases, default) -> List.for_all (fun (_, body) -> pure_lianst_result body) cases &&
    (match default with None -> true | Some body -> pure_lianst_result body)
  | CMatchTry (i, body, handle) -> pure_lianst_result body && pure_lianst_result handle
  | CApp (imm, args) -> false
  | CField _ | CArrayGet _ | CMakeBlock _ -> false (* With better analysis, could determine as true in some cases *)

and pure_lianst_result linast = match linast.desc with
  | LLet (_, _, body, rest) -> pure_comp_result body && pure_lianst_result rest
  (* Currently can't analyse functions, so just look at rest since can't do anything with function definitions *)
  | LLetRec (_, rest) -> pure_lianst_result rest
  | LSeq (comp, lin) -> pure_comp_result comp && pure_lianst_result lin
  | LCompound comp -> pure_comp_result comp