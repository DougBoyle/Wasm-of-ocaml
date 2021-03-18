(* Combines constant and copy propagation *)
open Linast
let replaced_idents = (Ident.Tbl.create 50 : imm_expr_desc Ident.Tbl.t)

(* Ideally want this to happen after body evaluated, just means extra passes needed *)
let enter_linast linast = (match linast.desc with
  (* Don't try to propagate constants on Mut idents *)
  | LLet(id, (Local | Export), body, rest) -> (match body.c_desc with
    (* Only immediate constants, binop(im1, im2) first reduced by constant folding *)
    | CImm {i_desc} -> Ident.Tbl.add replaced_idents id i_desc
    | _ -> ())
  (* Also done for recursive LLetRec in case of useless binding like 'let rec f = g' *)
  | LLetRec(binds, _) -> List.iter
    (function
      | (id, (Local | Export), {c_desc = CImm {i_desc}}) ->
        Ident.Tbl.add replaced_idents id i_desc
      | _ -> ()) binds
  | _ -> () (* LLetRec is always a function so no constants to optimise *)
  ); linast

let map_imm imm = match imm.i_desc with
  | ImmIdent id ->
    (match Ident.Tbl.find_opt replaced_idents id with Some i_desc -> {imm with i_desc} | None -> imm)
  | _ -> imm

let leave_linast linast = (match linast.desc with
  (* remove from table, no effect if wasn't in table to begin with *)
  | LLet(id, _, body, rest) -> Ident.Tbl.remove replaced_idents id
  | _ -> ()); linast

let optimise linast =
  Ident.Tbl.clear replaced_idents;
  (LinastMap.create_mapper ~map_imm ~enter_linast ~leave_linast ()) linast
