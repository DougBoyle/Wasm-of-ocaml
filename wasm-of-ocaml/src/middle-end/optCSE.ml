(* Can do much more complex analysis by first doing analysis pass to propagate annotations,
   and by tracking if something could actually have been assigned to between declaration/use.
   e.g. y = field 0 x; ...; z = field 0 x
   even if x has mutable fields, can Apply CSE if no field assignments between.
   Would mean keeping a running set of valid expressions and removing as they (potentially) become invalid *)

(* Need to keep a HashTbl of precomputed expressions
   Equality doesn't depend on annotations/location/environment etc. so need to generate custom HashTbl
   See: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html*)

open Linast

module CompoundHash = struct
    type t = Linast.compound_expr_desc (* keys of the table *)
    let equal c1 c2 = match (c1, c2) with
        | CImm {desc=desc1}, CImm{desc=desc2} -> desc1 = desc2
       (* | CMatchFail i, CMatchFail j -> i = j *) (* Can never actually occur as a binding *)
        | CUnary(op1, {desc=desc1}), CUnary(op2, {desc=desc2}) -> op1 = op2 && desc1 = desc2
        | CBinary (op1, {desc=desc1}, {desc=desc2}), CBinary (op2, {desc=desc3}, {desc=desc4}) ->
          op1 = op2 && desc1 = desc3 && desc2 = desc4
        | CSetField ({desc=desc1}, i, {desc=desc2}), CSetField ({desc=desc3}, j, {desc=desc4}) ->
          i = j && desc1 = desc3 && desc2 = desc4
        | CField ({desc=desc1}, i), CField ({desc=desc2}, j) -> i = j && desc1 = desc2
        | CArraySet ({desc=desc1}, {desc=desc2}, {desc=desc3}), CArraySet ({desc=desc4}, {desc=desc5}, {desc=desc6}) ->
          desc1 = desc4 && desc2 = desc5 && desc3 = desc6
        | CArrayGet ({desc=desc1}, {desc=desc2}), CArrayGet ({desc=desc3}, {desc=desc4}) ->
          desc1 = desc3 && desc2 = desc4
        | CMakeBlock (i, imms1), CMakeBlock(j, imms2) -> i = j && List.for_all
          (fun (({desc=desc1} : imm_expr), ({desc=desc2} : imm_expr)) -> desc1 = desc2) (List.combine imms1 imms2)
        | CGetTag {desc=desc1}, CGetTag {desc=desc2} -> desc1 = desc2
        (* Due to unique indents, would need alpha-conversion test to determine function equality *)
        | _ -> false (* TODO: Why not define equality for Linasts? Gets too exhaustive?
                              Note - will end up storing compounds that cannot be used?
                              Suggests need to test before logging something as reusable *)
    (* c1 = c2 must guarentee that hash c1 = hash c2 *)
    let hash = function
        | CImm {desc} -> Hashtbl.hash "Imm" lxor Hashtbl.hash desc
      (*  | CMatchFail i ->  *)
        | CUnary(op, {desc}) -> Hashtbl.hash "Unary" lxor Hashtbl.hash op lxor Hashtbl.hash desc
        | CBinary (op, {desc=desc1}, {desc=desc2}) ->
          Hashtbl.hash "Binary" lxor Hashtbl.hash op lxor Hashtbl.hash desc1 lxor Hashtbl.hash desc2
        | CSetField ({desc=desc1}, i, {desc=desc2}) ->
          Hashtbl.hash "SetField" lxor Hashtbl.hash i lxor Hashtbl.hash desc1 lxor Hashtbl.hash desc2
        | CField ({desc}, i) -> Hashtbl.hash "Field" lxor Hashtbl.hash i lxor Hashtbl.hash desc
        | CArraySet ({desc=desc1}, {desc=desc2}, {desc=desc3}) ->
          Hashtbl.hash "ArraySet" lxor Hashtbl.hash desc1 lxor Hashtbl.hash desc2 lxor Hashtbl.hash desc3
        | CArrayGet ({desc=desc1}, {desc=desc2}) ->
          Hashtbl.hash "ArrayGet" lxor Hashtbl.hash desc1 lxor Hashtbl.hash desc2
        | CMakeBlock (i, imms) -> List.fold_left
          (fun hash ({desc} : imm_expr) -> hash lxor Hashtbl.hash desc) (Hashtbl.hash i lxor Hashtbl.hash "MakeBlock") imms
        | CGetTag {desc} -> Hashtbl.hash "GetTag" lxor Hashtbl.hash desc
        | desc -> Hashtbl.hash desc (* Don't want to be putting these items in the HashTbl *)
end

module CompoundHashtbl = Hashtbl.Make(CompoundHash)

(* Looks like Ident.Tbl allows it to be mutable, but Ident.tbl type and add etc. are for immutable version *)
(* 50 Chosen arbitrarily *)
let replaced_idents = (Ident.Tbl.create 50 : Ident.t Ident.Tbl.t)
let common_expressions = (CompoundHashtbl.create(50) : Ident.t CompoundHashtbl.t)

let enter_linast linast = (match linast.desc with
  | LLet(id, _, body, rest) -> (match CompoundHashtbl.find_opt common_expressions body.desc with
    | Some id' -> Ident.Tbl.add replaced_idents id id'
    | None -> if Pure.pure_comp_result body then
      CompoundHashtbl.add common_expressions body.desc id)
  | _ -> () (* Can't reuse function definitions due to unique idents as arguments, so leave LLetRec *)
  ); linast

let map_imm (imm : Linast.imm_expr) = match imm.desc with
  | ImmIdent id ->
    (match Ident.Tbl.find_opt replaced_idents id with Some id' -> {imm with desc=ImmIdent id'} | None -> imm)
  | _ -> imm

(* Opposite of enter_linast as binding goes out of scope *)
(* Removing from replaced_idents is just to keep table small, idents unique so not needed for correctness *)
(* Ensures bindings that may not happen (e.g. if statements) don't leak into later parts.
   Much more complex graph-based methods needed to determine if something gets bound on all paths or not *)
let leave_linast linast = (match linast.desc with
  | LLet(id, _, body, rest) -> (match CompoundHashtbl.find_opt common_expressions body.desc with
    (* This was being used as a common expression, remove since no longer in scope *)
    | Some id' -> if id = id' then CompoundHashtbl.remove common_expressions body.desc
      else Ident.Tbl.remove replaced_idents id (* Replacement rule no longer needed, ident out of scope *)
    (* Some Imm within body has been replaced, hence compound not found in table.
       No way to remove the original expression stored in the table, just try to remove possible ident replacement *)
    | None -> Ident.Tbl.remove replaced_idents id)
  | _ -> ()
  ); linast (* In case where this binding was replaced, now useless binding removed by dead assignment pass *)

let optimise linast =
  Ident.Tbl.clear replaced_idents;
  CompoundHashtbl.clear common_expressions;
  (* TODO: Put actual mappers in *)
  (LinastMap.create_mapper ~map_imm ~enter_linast ~leave_linast ()) linast
