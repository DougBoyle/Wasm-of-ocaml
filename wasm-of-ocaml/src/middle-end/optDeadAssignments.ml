(* Need to be careful about Idents bound within body but used in 'handle' block.
   e.g. Try(i, Let y = 5 in exit i, y)
   y appears to be a useless assignment until handler evaluated.
   For this reason, linastMap visits handler before body - May need to just handle specially if other order needed
*)
(* TODO: Is it useful to remove dead side effects? e.g. Seq( (), e) -> e. Likely do in other pass *)
open Linast

let alive = ref Ident.Set.empty
let mark id = alive := Ident.Set.add id (!alive)
(* TODO: Work out how to replace unused but impure bindings with sequences. Would need to take the whole linast term *)
let is_dead (id, (compound : compound_expr)) =
  if Ident.Set.mem id (!alive) then false else List.mem Pure (!(compound.annotations))

let map_imm (imm : Linast.imm_expr) = match imm.desc with
  (* Ident appears in a compound so mark as used. May remove in later passes if that compound is unused.
     Suggests a DAG structure would actually be more effective, but harder to implement.
     Currently won't spot 'mutually useless' idents that only interact with each other. *)
  | ImmIdent id -> mark id; imm
  | _ -> imm

(* Special case for mutable idents (used by tail call optimisation) where they need to be marked,
   as they are stored as idents not immediates *)
let enter_compound (compound : compound_expr) = match compound.desc with
  | CAssign (id, _) -> mark id; compound
  | _ -> compound

(* Only change is to remove some Let bindings after processing body, so just need leave_linast *)
let leave_linast linast = match linast.desc with
  (* Must not remove exported idents *)
  | LLet (id, Local, compound, body) when is_dead (id, compound) -> body
  (* Top of functions sometimes introduce bindings of a variable to itself, since
     binding is implicit in CApp and function starts with a case statement.
     Note that Ident.same != Ident.equal. Ident.equal only checks names not stamps. *)
  | LLet(id, _, {desc = CImm {desc = ImmIdent id'}}, body) when Ident.same id id' -> body
  (* all binds/body have been processed at this point, so can remove any unused binds now *)
  | LLetRec (binds, body) ->
    let new_binds = List.filter (function (id, Local, comp) -> not(is_dead (id, comp)) | _ -> true) binds
    in if List.length new_binds = List.length binds then linast (* No change made *)
    else (match new_binds with [] -> body | _ -> {linast with desc=LLetRec(new_binds, body)})
  | _ -> linast

let optimise linast =
  alive := Ident.Set.empty;
  (LinastMap.create_mapper ~map_imm ~enter_compound ~leave_linast ()) linast
