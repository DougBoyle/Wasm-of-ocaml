(* Test if assignments can be safely removed. *)
(* Neater but less efficient to do as an analysis pass? *)
(* Ideally, if impure then optDeadAssignments would turn it into a sequence rather than a binding *)
(* TODO: Using analysis attached to structures, could likely do smarter analysis i.e. add Pure/Impure flags
         Find an example where this would be beneficial. *)
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
   (*false*) (* TODO: Would be useful to 'scope' purity i.e. ignore Fail i  in body. *)
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
