open Linast
open LinastUtils
(* For now just inline any Local function that is only used once.
   Avoid trying to inline recursive functions.
   i.e. could have been written as (x -> ...)e
*)
(* Searching for functions only used once is very similar to dead assignment analysis, some duplication?
   Only inline if the only usage is a single application, so count adds 2 and CApp removes 1.
   TODO: Find neater solution for this *)
let uses = ref (Ident.empty : ((int * Ident.t list * linast_expr) Ident.tbl))
let count id =
  try
    let previous, params, body = Ident.find_same id (!uses) in
    uses := Ident.add id (previous + 2, params, body) (Ident.remove id (!uses))
  with Not_found -> ()
let mark_app id =
  try
    let previous, params, body = Ident.find_same id (!uses) in
    uses := Ident.add id (previous - 1, params, body) (Ident.remove id (!uses))
  with Not_found -> ()
let remove_id id = uses := Ident.remove id (!uses)

let map_imm (imm : Linast.imm_expr) = match imm.desc with
  | ImmIdent id -> count id; imm
  | _ -> imm

(* Special case for mutable idents (used by tail call optimisation) where they need to be marked,
   as they are stored as idents not immediates *)
(* Idents assigned to are never function arguments, so can ignore them *)
let enter_compound (compound : compound_expr) = match compound.desc with
  | CApp({desc=ImmIdent id}, _) -> mark_app id; compound
  | _ -> compound

let enter_linast linast = match linast.desc with
  (* If only use of function is inline, Dead Assignment Elimination will remove it *)
  | LLet (id, Local, {desc = CFunction (args, body)}, _) -> uses := Ident.add id (0, args, body) (!uses); linast
  | _ -> linast

let substitue mapping (imm : Linast.imm_expr) = match imm.desc with
  | ImmIdent id -> (match List.assoc_opt id mapping with Some imm' -> imm' | None -> imm)
  | _ -> imm

(* Rewriting may also need to create a new function if f is under/over applied *)
let inline_function args parameters body =
  if (List.length args) = (List.length parameters) then
    let mapping = List.combine parameters args in
    (LinastMap.create_mapper ~map_imm:(substitue mapping) ()) body
  else if (List.length args > List.length parameters) then
    let applied_args, rest = Linearise.take (List.length parameters) args in
    let mapping = List.combine parameters applied_args in
    let body' = (LinastMap.create_mapper
        ~map_imm:(substitue mapping) ()) body in
    let f = Ident.create_local "f" in
    (* TODO: Rewrite to bind f to body' *)
    OptConstants.rewrite_tree (LinastExpr.mklet f Local) body'
      (LinastExpr.compound (Compound.app (Imm.id f) rest))
  else
    let applied_params, rest = Linearise.take (List.length args) parameters in
    let mapping = List.combine applied_params args in
    let body' = (LinastMap.create_mapper
        ~map_imm:(substitue mapping) ()) body in
    LinastExpr.compound (Compound.mkfun rest body')

let leave_linast linast =
  try (match linast.desc with
  | LLet (id, Local, {desc = CFunction _}, _) -> remove_id id; linast
  | LLet (id, export, {desc = CApp ({desc=ImmIdent f}, args)}, rest) ->
    let (count, params, body) = Ident.find_same f (!uses) in
    if count = 1 then
      OptConstants.rewrite_tree (LinastExpr.mklet id export)
       (inline_function args params body) rest
    else linast
  | LSeq({desc = CApp ({desc=ImmIdent f}, args)}, rest) ->
    let (count, params, body) = Ident.find_same f (!uses) in
    if count = 1 then
      OptConstants.rewrite_tree LinastExpr.seq (inline_function args params body) rest
    else linast
  | LCompound {desc = CApp ({desc=ImmIdent f}, args)} ->
    let (count, params, body) = Ident.find_same f (!uses) in
    if count = 1 then
      inline_function args params body
    else linast
  | _ -> linast)
  with Not_found -> linast

let optimise linast =
  (LinastMap.create_mapper ~map_imm ~enter_linast ~enter_compound ~leave_linast ()) linast
