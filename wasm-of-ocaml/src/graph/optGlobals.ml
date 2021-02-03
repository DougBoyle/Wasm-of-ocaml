(*
  Global variables that are only touched in the main function and aren't exported
  can be replaced with temporaries. Main is always the last function.
  All other globals then need indexes updated, including exports of globals.
  Then also add a local to main to replace the global.

  Temporaries are introduced as globals due to all top-level bindings being made into globals.
  This helps reduce the size of closures in cases such as:
   let a = 5
   let f x = x + a
   let a = 10
  In this situation, 'a' is not exported, but by making it a global it doesn't need to be put in closures

  Alternative approach was to tag temporary variables in Linast, but since the tags are only meaningful
  in the main function, didn't seem like the best solution.
*)
open Graph
open Wasm
open Utils

let rec is_global_instr i {it} = match it with
  | Block (_, body) | Loop (_, body) -> List.exists (is_global_instr i) body
  | If (_, body1, body2) -> List.exists (is_global_instr i) body1 || List.exists (is_global_instr i) body2
  | GlobalGet {it=j} | GlobalSet {it=j} when i = j -> true
  | _ -> false

let is_global_used i funcs exports =
  (* Exported (lots of unnecessary wrappers around export type) *)
  List.exists
  (function ({it={Ast.edesc={it = Ast.GlobalExport {it = j}}}} : Ast.export)
    when i = j -> true | _ -> false) exports ||
  (* Used by another function *)
  List.exists (fun {body} -> List.exists (is_global_instr i) body) funcs

(* mapping maps old_index to new_index for all other globals *)
let rec update_globals mapping ({it} as instr) = match it with
  | Block (typ, body) -> {instr with it = Block(typ, List.map (update_globals mapping) body)}
  | Loop (typ, body) -> {instr with it = Loop(typ, List.map (update_globals mapping) body)}
  | If (typ, body1, body2) ->
    {instr with it = If(typ, List.map (update_globals mapping) body1, List.map (update_globals mapping) body2)}
  | GlobalGet ({it=i} as var) -> {instr with it = GlobalGet {var with it = List.assoc i mapping}}
  | GlobalSet ({it=i} as var) -> {instr with it = GlobalSet {var with it = List.assoc i mapping}}
  | _ -> instr

let rec replace_globals mapping ({it} as instr) = match it with
  | Block (typ, body) -> {instr with it = Block(typ, List.map (replace_globals mapping) body)}
  | Loop (typ, body) -> {instr with it = Loop(typ, List.map (replace_globals mapping) body)}
  | If (typ, body1, body2) ->
    {instr with it = If(typ, List.map (replace_globals mapping) body1, List.map (replace_globals mapping) body2)}
  | GlobalGet ({it=i} as var) ->
    (match List.assoc_opt i mapping with
      | Some j -> {instr with it = LocalGet {var with it = j}}
      | None -> instr)
  | GlobalSet ({it=i} as var) ->
    (match List.assoc_opt i mapping with
      | Some j -> {instr with it = LocalSet ({var with it = j})}
      | None -> instr)
  | _ -> instr

let rec update_export mapping (({it={name; edesc}} as export) : Ast.export) =
  let open Wasm.Ast in
  match edesc.it with
    | GlobalExport ({it = i} as var) ->
      {export with it = {name; edesc={edesc with it = GlobalExport {var with it = List.assoc i mapping}}}}
    | _ -> export

let optimise ({funcs; globals; exports} as module_) =
  let other_funcs, main = split_last [] funcs in
  (* next_idx is index in output, i is original index *)
  let rec make_mapping (map, globals, removed) next_idx i = function
    | [] -> (map, List.rev globals, removed)
    | g::rest ->
      if is_global_used i other_funcs exports
      then make_mapping ((i, next_idx)::map, g::globals, removed) (Int32.add next_idx 1l) (Int32.add i 1l) rest
      else make_mapping (map, globals, i::removed) next_idx (Int32.add i 1l) rest in
  let mapping, new_globals, removed = make_mapping ([], [], []) 0l 0l globals in
  if removed = []
  then
    module_
  else
    let local_offset = Int32.of_int (List.length main.locals) in
    (* Replace globals with locals in main (and increase the number of locals it uses) *)
    let replacements = List.mapi (fun i global -> (global, Int32.add (Int32.of_int i) local_offset)) removed in
    let new_main_body = List.map (replace_globals replacements) main.body in
    let new_main = {main with
      body=new_main_body;
      locals=(List.init (List.length replacements) (fun _ -> Types.I32Type)) @ main.locals} in
    (* Update indexes of globals in other functions to account for some being removed *)
    let new_funcs = List.map
      (fun ({body} as f) -> {f with body = List.map (update_globals mapping) body}) (other_funcs @ [new_main]) in
    (* Do the same update to the list of exports *)
    let new_exports = List.map (update_export mapping) exports in
    {module_ with exports=new_exports; funcs=new_funcs; globals=new_globals}
