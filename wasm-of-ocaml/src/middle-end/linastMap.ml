(* utility functions for mapping over a Linast expression, used for optimisation passes *)
open Linast

(* Makes ordering of changes explicit i.e. enter_linast -> enter_compound -> map_imm -> leave_compound -> leave_linast
   e.g. if map_imm does renaming, have a function for compounds both before/after renaming happens *)
type mappers = {map_imm : Linast.imm_expr -> Linast.imm_expr;
  enter_compound : Linast.compound_expr -> Linast.compound_expr;
  leave_compound : Linast.compound_expr -> Linast.compound_expr;
  enter_linast : Linast.linast_expr -> Linast.linast_expr;
  leave_linast : Linast.linast_expr -> Linast.linast_expr;}

let rec map_compound_expr mappers compound =
  let compound' = mappers.enter_compound compound in
  let new_desc = match compound'.desc with
    | CImm imm -> CImm (mappers.map_imm imm)
    | CUnary (op, imm) -> CUnary (op, mappers.map_imm imm)
    | CBinary (op, imm1, imm2) -> CBinary (op, mappers.map_imm imm1, mappers.map_imm imm2)
    | CSetField (imm1, idx, imm2) -> CSetField (mappers.map_imm imm1, idx, mappers.map_imm imm2)
    | CField (imm, idx) -> CField (mappers.map_imm imm, idx)
    | CArraySet (imm1, imm2, imm3) -> CArraySet (mappers.map_imm imm1, mappers.map_imm imm2, mappers.map_imm imm3)
    | CArrayGet (imm1, imm2) -> CArrayGet (mappers.map_imm imm1, mappers.map_imm imm2)
    | CMakeBlock (tag, imms) -> CMakeBlock (tag, List.map mappers.map_imm imms)
    | CGetTag imm -> CGetTag (mappers.map_imm imm)
    | CIf (cond, thenbody, elsebody) ->
      CIf (mappers.map_imm cond, map_linast_expr mappers thenbody, map_linast_expr mappers elsebody)
    | CWhile (cond, body) -> CWhile (map_linast_expr mappers cond, map_linast_expr mappers body)
    | CFor (id, start, finish, dir, body) ->
      CFor (id, mappers.map_imm start, mappers.map_imm finish, dir, map_linast_expr mappers body)
    | CSwitch (imm, cases, default) ->
      CSwitch (mappers.map_imm imm, List.map (fun (i, body) -> (i, map_linast_expr mappers body)) cases,
               Option.map (map_linast_expr mappers) default)
    | CMatchTry (i, body, handle) ->
      (* Due to scoping hack, handler evaluated first. Ensures bindings never incorrectly identified as unused.
         May need to rewrite if other order ever needed. *)
      (* Not actually an issue until pattern matching optimised further. Currently, try/handle obey scoping *)
      let handle = map_linast_expr mappers handle in
      CMatchTry (i, map_linast_expr mappers body, handle)
    | CApp (imm, args) -> CApp(mappers.map_imm imm, List.map mappers.map_imm args)
    | CFunction (args, body) -> CFunction(args, map_linast_expr mappers body)
  in mappers.leave_compound {compound' with desc=new_desc}

and map_linast_expr mappers linast =
  let linast' = mappers.enter_linast linast in
  let new_desc = match linast'.desc with
    | LLet (id, flag, body, rest) ->
      LLet (id, flag, map_compound_expr mappers body, map_linast_expr mappers rest)
    | LLetRec (binds, rest) ->
      LLetRec (List.map (fun (id, flag, body) -> (id, flag, map_compound_expr mappers body)) binds,
               map_linast_expr mappers rest)
    | LSeq (comp, lin) -> LSeq(map_compound_expr mappers comp, map_linast_expr mappers lin)
    | LCompound (comp) -> LCompound(map_compound_expr mappers comp)
  in mappers.leave_linast {linast' with desc=new_desc}

let identity x = x

let create_mapper ?(map_imm=identity) ?(enter_compound=identity) ?(leave_compound=identity)
  ?(enter_linast=identity) ?(leave_linast=identity) () =
  map_linast_expr {map_imm; enter_compound; leave_compound; enter_linast; leave_linast}
