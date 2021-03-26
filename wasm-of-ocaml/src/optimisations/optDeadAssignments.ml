(* Need to be careful about Idents bound within body but used in 'handle' block.
   e.g. Try(i, Let y = 5 in exit i, y)
   y appears to be a useless assignment until handler evaluated.
   For this reason, linastMap visits handler before body - May need to just handle specially if other order needed
*)
open Linast
open LinastUtils

let alive = ref Ident.Set.empty
let mark id = alive := Ident.Set.add id (!alive)
(* If a term is unused but impure, LocalSet at Wasm level will be identified as dead and removed *)
let is_dead (id, compound) =
  if Ident.Set.mem id (!alive) then false else List.mem Pure (!(compound.c_annotations))

let map_imm imm = match imm.i_desc with
  (* Ident appears in a compound so mark as used. May remove in later passes if that compound is unused.
     Suggests a DAG structure would actually be more effective, but harder to implement.
     Currently won't spot 'mutually useless' idents that only interact with each other. *)
  | ImmIdent id -> mark id; imm
  | _ -> imm

(* Special case for mutable idents (used by tail call optimisation) where they need to be marked,
   as they are stored as idents not immediates *)
let enter_compound compound = match compound.c_desc with
  | CAssign (id, _) -> mark id; compound
  | _ -> compound

(* Only change is to remove some Let bindings after processing body, so just need leave_linast *)
let leave_linast linast = match linast.desc with
  (* Special case where value is bound to a temporary and temporary assigned only to an exported varaible.
     Temporary is used and variable is exported, so wouldn't otherwise be optimised.
     Order swapped so that it may be exported in future *)
  | LLet (id1, Local, compound,
    ({desc = LLet(id2, Export, {c_desc=CImm {i_desc=ImmIdent id3}}, rest)} as linast2))
    when Ident.same id1 id3 ->
    {linast with desc=LLet(id2, Export, compound,
    {linast2 with desc=LLet(id1, Local, Compound.imm (Imm.id id2), rest)})}
  (* Top of functions sometimes introduce bindings of a variable to itself, since
     binding is implicit in CApp and function starts with a case statement.
     Note that Ident.same != Ident.equal. Ident.equal only checks names not stamps. *)
  | LLet(id, _, {c_desc = CImm {i_desc = ImmIdent id'}}, body) when Ident.same id id' -> body
  (* Must not remove exported idents *)
  | LLet (id, Local, compound, body) when is_dead (id, compound) -> body
  | LLetRec (binds, body) ->
    let new_binds = List.filter (function (id, Local, comp) -> not(is_dead (id, comp)) | _ -> true) binds
    in if List.length new_binds = List.length binds then linast (* No change made *)
    else (match new_binds with [] -> body | _ -> {linast with desc=LLetRec(new_binds, body)})
  (* If first part of a sequence is pure, can remove it *)
  | LSeq (c, body) when List.mem Pure (!(c.c_annotations)) -> body
  | _ -> linast

let optimise linast =
  alive := Ident.Set.empty;
  (LinastMap.create_mapper ~map_imm ~enter_compound ~leave_linast ()) linast
