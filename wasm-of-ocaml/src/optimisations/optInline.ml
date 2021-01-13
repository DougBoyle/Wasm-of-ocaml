open Linast
open LinastUtils
open Utils

(*
  Heuristics to decide which functions to inline.
  Table of properties kept for each defined function.
  Heuristics based on (simplified version of): http://titanium.cs.berkeley.edu/papers/bonachea-method-inlining.pdf
  TODO: Is it worth inlining under-applications, still have to create a new function but may be able to further inline
*)

(* TODO: Also want to track how many function calls the body of a function makes? (then can't use Mapper?) *)
type func_info = {
  size: int;
  local: bool;
  arity: int;
  parameters: Ident.t list;
  body: linast_expr;
  applications: int; (* fully or over applied *)
  under_applications: int;
  other_uses: int; (* passed as a variable *)
}

let inline_info = ref (Ident.empty : func_info Ident.tbl)

let rec estimate_size linast = match linast.desc with
  | LLet(_, _, compound, rest) -> (estimate_size_compound compound) + (estimate_size rest)
  | LLetRec(binds, rest) -> List.fold_right
    (fun (_, _, compound) tot -> tot + (estimate_size_compound compound)) binds (estimate_size rest)
  | LSeq(compound, rest) -> (estimate_size_compound compound) + (estimate_size rest)
  | LCompound c -> estimate_size_compound c

and estimate_size_compound (compound : compound_expr) = match compound.desc with
  | CIf (_, body1, body2) | CWhile (body1, body2)
  | CMatchTry (_, body1, body2) -> 1 + (estimate_size body1) + (estimate_size body2)
  | CFor (_, _, _, _, body) | CFunction (_, body) -> 1 + (estimate_size body)
  | CSwitch (_, cases, default) -> List.fold_right (fun (_, body) tot -> (estimate_size body) + tot) cases
    (match default with None -> 1 | Some body -> (estimate_size body) + 1)
  | _ -> 1

(* --------------------- Decide to inline or not -------------------- *)
(* Only used once and inlining will remove the original declaration, so doesn't increase code size *)
(* This only has a negative effect when a very large function is inlined into a very small function
   that rarely uses the larger function. Now the stack frame for the small function will often be unnecessarily large *)

(* TODO: Since multiple passes are performed, budget should be based on code size during 1st pass.
     Otherwise, code could grow on every pass and get far too large. *)
let size_limit = ref 0
let budget = ref 0

let can_remove_if_inlined {local; applications; under_applications; other_uses} =
  (applications + under_applications = 1) && local && (other_uses = 0)

(* Typically getters of reference values or function that just calls another function *)
let function_is_very_small {size} = size < 5 (* 5 chosen arbitrarily *)

(* TODO: Track leave nodes of call graph too i.e. doesn't make any other function calls *)
let small_enough_and_in_budge {size} = size < 20 && (!budget) > 20

let should_inline info = List.exists (fun heuristic -> heuristic info)
  [can_remove_if_inlined; function_is_very_small; small_enough_and_in_budge]

(* --------------------- Identify uses of function -------------------- *)

let count id =
  try
    let info = Ident.find_same id (!inline_info) in
    inline_info := Ident.add id {info with other_uses = info.other_uses + 1} (Ident.remove id (!inline_info))
  with Not_found -> ()

let mark_app id num_args =
  try
    let info = Ident.find_same id (!inline_info) in
    if num_args < info.arity
    then inline_info := Ident.add id
        {info with other_uses = info.other_uses - 1; applications = info.under_applications + 1}
        (Ident.remove id (!inline_info))
    else inline_info := Ident.add id
      {info with other_uses = info.other_uses - 1; applications = info.applications + 1}
      (Ident.remove id (!inline_info))
  with Not_found -> ()

let remove_id id =
  inline_info := Ident.remove id (!inline_info)

let map_imm (imm : Linast.imm_expr) = match imm.desc with
  | ImmIdent id -> count id; imm
  | _ -> imm

(* Special case for mutable idents (used by tail call optimisation) where they need to be marked,
   as they are stored as idents not immediates *)
(* Idents assigned to are never function arguments, so can ignore them *)
let enter_compound (compound : compound_expr) = match compound.desc with
  | CApp({desc=ImmIdent id}, args) -> mark_app id (List.length args); compound
  | _ -> compound

let enter_linast linast = match linast.desc with
  (* If only use of function is inline, Dead Assignment Elimination will remove it *)
  | LLet (id, export, {desc = CFunction (parameters, body)}, _) ->
    inline_info := Ident.add id
      {applications = 0; under_applications = 0; other_uses = 0; arity = List.length parameters;
      parameters; body; local=(export = Local); size = estimate_size body}
      (!inline_info); linast
  | _ -> linast

(* Important to copy the annotations in each case, so that analysis on inlined functions doesn't
   affect the annotations on the original function *)
let substitue (mapping : (Ident.t * imm_expr) list) (imm : imm_expr) : (imm_expr) = match imm.desc with
  | ImmIdent id ->
    (match List.assoc_opt id mapping with
      | Some imm' -> {imm' with annotations = ref (!(imm'.annotations))}
      | None -> {imm with annotations = ref (!(imm.annotations))})
  | _ -> {imm with annotations = ref (!(imm.annotations))}
let copy_compound (compound : compound_expr) = {compound with annotations = ref (!(compound.annotations))}
let copy_linast linast = {linast with annotations = ref (!(linast.annotations))}

(* Rewriting may also need to create a new function if f is under/over applied *)
let inline_function args {arity; parameters; body; size} =
  (* estimate increase in program size *)
  budget := (!budget) - size;
  if (List.length args) = arity then
    let mapping = List.combine parameters args in
    (LinastMap.create_mapper ~map_imm:(substitue mapping)
      ~leave_compound:copy_compound ~leave_linast:copy_linast ()) body
  else if (List.length args) > arity then
    let applied_args, rest = take (List.length parameters) args in
    let mapping = List.combine parameters applied_args in
    let body' = (LinastMap.create_mapper ~map_imm:(substitue mapping)
      ~leave_compound:copy_compound ~leave_linast:copy_linast ()) body in
    let f = Ident.create_local "f" in
    OptConstants.rewrite_tree (LinastExpr.mklet f Local) body'
      (LinastExpr.compound (Compound.app (Imm.id f) rest))
  else
    let applied_params, rest = take (List.length args) parameters in
    let mapping = List.combine applied_params args in
    let body' = (LinastMap.create_mapper ~map_imm:(substitue mapping)
      ~leave_compound:copy_compound ~leave_linast:copy_linast ()) body in
    LinastExpr.compound (Compound.mkfun rest body')

let leave_linast linast =
  try (match linast.desc with
  | LLet (id, Local, {desc = CFunction _}, _) -> remove_id id; linast
  | LLet (id, export, {desc = CApp ({desc=ImmIdent f}, args)}, rest) ->
    let info = Ident.find_same f (!inline_info) in
    if should_inline info then
      OptConstants.rewrite_tree (LinastExpr.mklet id export)
       (inline_function args info) rest
    else linast
  | LSeq({desc = CApp ({desc=ImmIdent f}, args)}, rest) ->
    let info = Ident.find_same f (!inline_info) in
    if should_inline info then
      OptConstants.rewrite_tree LinastExpr.seq (inline_function args info) rest
    else linast
  | LCompound {desc = CApp ({desc=ImmIdent f}, args)} ->
    let info = Ident.find_same f (!inline_info) in
    if should_inline info then
      inline_function args info
    else linast
  | _ -> linast)
  with Not_found -> linast

let optimise linast =
  (* Calculate budget on first pass, paper suggests allowing 50% increase *)
  if (!size_limit) = 0
  then (size_limit := (estimate_size linast)*3/2; budget := (!size_limit)/3)
  else budget := (!size_limit) - (estimate_size linast);
  (LinastMap.create_mapper ~map_imm ~enter_linast ~enter_compound ~leave_linast ()) linast
