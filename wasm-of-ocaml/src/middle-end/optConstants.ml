(* CSE only replaces Idents with other Idents, this can replace Idents with Constants.
   i.e. let x = 5 in let y = x in ... => let x = 5 in let y = 5 in ...
   If x was only being used to assign y, will now be removed as a dead assignment
   TODO: Constant folding i.e. simplifying unops/binops etc. Complex analysis could optimise GetTag too. *)
open Linast

let constant_idents = (Ident.Tbl.create 50 : Asttypes.constant Ident.Tbl.t)

let enter_linast linast = (match linast.desc with
  | LLet(id, _, body, rest) -> (match body.desc with
    (* Only immediate constants, binop(im1, im2) first reduced by constant folding *)
    | CImm {desc=ImmConst c} -> Ident.Tbl.add constant_idents id c
    | _ -> ())
  | _ -> () (* LLetRec is always a function so no constants to optimise *)
  ); linast

let map_imm (imm : Linast.imm_expr) = match imm.desc with
  | ImmIdent id ->
    (match Ident.Tbl.find_opt constant_idents id with Some c -> {imm with desc=ImmConst c} | None -> imm)
  | _ -> imm

(* Not necessary as ident won't be reused once out of scope, but helps keep table small
   TODO: By doing this, can't optimise bindings passed through an exit to a handler
         (not yet done by pattern match).
         Implicitly passing out values is starting to look like an issue, any way to avoid this scope issue? *)
let leave_linast linast = (match linast.desc with
  | LLet(id, _, body, rest) -> (match body.desc with
    | CImm {desc=ImmConst c} -> Ident.Tbl.remove constant_idents id
    | _ -> ())
  | _ -> ()
  ); linast

let optimise linast =
  Ident.Tbl.clear constant_idents;
  (LinastMap.create_mapper ~map_imm ~enter_linast ~leave_linast ()) linast
