(* Test if assignments can be safely removed. *)
(* Neater but less efficient to do as an analysis pass? *)
(* Ideally, if impure then optDeadAssignments would turn it into a sequence rather than a binding *)
(* TODO: Using analysis attached to structures, could likely do smarter analysis i.e. add Pure/Impure flags
         Find an example where this would be beneficial. *)
open Linast
open LinastUtils

(* Needed since ident/const are pure, but fail isn't since it compiles to a jump *)
(* Should only actually appear as Compound.Imm (ImmMatchFail) -- Suggests Fail should be moved to compound level?! *)
let is_imm_pure (imm : Linast.imm_expr) = match imm.desc with
  | ImmMatchFail _ -> false
  | _ -> true

(* Test to see if a compound value has any side effects *)
let rec is_comp_pure (comp : Linast.compound_expr) = match comp.desc with
  | CImm imm -> is_imm_pure imm
  (* Note - depends on set of expressions implemented using Unary/Binary. Currently all pure *)
  | CUnary (_, imm) -> is_imm_pure imm
  | CBinary (_, imm1, imm2) -> is_imm_pure imm1 && is_imm_pure imm2
  | CSetField _ -> false
  | CField (imm, _) -> is_imm_pure imm
  | CArraySet _ -> false
  | CArrayGet (imm1, imm2) -> is_imm_pure imm1 && is_imm_pure imm2
  | CMakeBlock (_, imms) -> List.for_all is_imm_pure imms
  | CGetTag imm -> is_imm_pure imm
  | CIf (imm, thenbody, elsebody) -> is_imm_pure imm && is_linast_pure thenbody && is_linast_pure elsebody
  (* Don't expect while statements to be pure, since then they're either useless or loop forever *)
  | CWhile (cond, body) -> is_linast_pure cond && is_linast_pure body
  | CFor (_, imm1, imm2, _, body) -> is_imm_pure imm1 && is_imm_pure imm2 && is_linast_pure body
  | CSwitch (imm, cases, default) -> is_imm_pure imm && List.for_all (fun (_, body) -> is_linast_pure body) cases &&
    (match default with None -> true | Some body -> is_linast_pure body)
  | CMatchTry (i, body, handle) -> false (* TODO: Would be useful to 'scope' purity i.e. ignore Fail i in this case. *)
  (* TODO: More complex analysis needed to track pure functions, suggests analysis pass required *)
  | CApp (imm, args) -> false (* No way to tell what the function does, need analysis pass before this *)
  | CFunction _ -> true (* Just declares a function *)

(* When body of compound contains a linast i.e. If(cond, body, else) - want to check if linast has any side-effects *)
and is_linast_pure linast = match linast.desc with
  | LLet (_, _, body, rest) -> is_comp_pure body && is_linast_pure rest
  (* Currently can't analyse functions, so just look at rest since can't do anything with function definitions *)
  | LLetRec (_, rest) -> is_linast_pure rest
  | LSeq (comp, lin) -> is_comp_pure comp && is_linast_pure lin
  | LCompound comp -> is_comp_pure comp
