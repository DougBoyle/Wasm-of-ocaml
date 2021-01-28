open Graph
open GraphUtils

(* TODO: Add unit tests for these functions! *)
let liveness_changing = ref false

let get_live_at_succ instr = List.fold_right (fun {live} set -> Set32.union (!live) set) (!(instr.succ)) Set32.empty

(* Steps in reverse instruction order, rather than directly traversing using pred/succ nodes.
   Could speed up by precomputing over basic blocks (would need new structure to identify/tack such blocks *)
(* Live = union of live at each successor + get - set/tee *)
let rec analyse_instr = function
  | {it=LocalGet {it=i}} as instr ->
    let new_live =  Set32.add i (get_live_at_succ instr) in
    if Set32.equal new_live (!(instr.live)) then () else (liveness_changing := true; instr.live := new_live)
  | {it=(LocalTee {it=i} | LocalSet {it=i})} as instr  ->
    let new_live = Set32.remove i (get_live_at_succ instr) in
    if Set32.equal new_live (!(instr.live)) then () else (liveness_changing := true; instr.live := new_live)
  | {it=(Block(_, body)|Loop(_, body))} ->
    List.iter analyse_instr (List.rev body);
  | {it=If(_, body1, body2)} ->
    List.iter analyse_instr (List.rev body1); List.iter analyse_instr (List.rev body2)
  | instr ->
    let new_live = get_live_at_succ instr in
    if Set32.equal new_live (!(instr.live)) then () else (liveness_changing := true; instr.live := new_live)

let rec analyse_liveness rev_body =
  List.iter analyse_instr rev_body;
  if (!liveness_changing) then ( liveness_changing := false; analyse_liveness rev_body)

(* No need to reset liveness analysis. Any which aren't valid on next pass will be removed, while rest remain correct *)
let rec optimise_instrs = function
  (* Look at successors, not that actual node (else would never be live) *)
  | ({it=LocalTee {it=i}} as instr)::rest ->
    if Set32.mem i (get_live_at_succ instr) then instr::(optimise_instrs rest)
    else (remove_instr instr; optimise_instrs rest)
  | ({it=LocalSet {it=i}} as instr)::rest ->
    if Set32.mem i (get_live_at_succ instr) then instr::(optimise_instrs rest)
    else {instr with it=Drop}::(optimise_instrs rest)
  | ({it=Block(typ, body)} as instr)::rest ->
    (* Unnecessarily recreates body if no changes *)
    {instr with it=Block(typ, optimise_instrs body)}::(optimise_instrs rest)
  | ({it=Loop(typ, body)} as instr)::rest ->
    {instr with it=Loop(typ, optimise_instrs body)}::(optimise_instrs rest)
  | ({it=If(typ, body1, body2)} as instr)::rest ->
    {instr with it=If(typ, optimise_instrs body1, optimise_instrs body2)}::(optimise_instrs rest)
  | instr::instrs -> instr::(optimise_instrs instrs)
  | [] -> []

(* Scan over function to find all locals which are accessed at least once. Remove all others.
   Cannot remove the first 2 locals as they are the function arguments. Currently, always 2, but
   check type to ensure compatibility if some functions modified to not take closure. *)
let rec used_locals set = function
  | [] -> set
  | ({it=LocalTee {it=i} | LocalSet {it=i} | LocalGet {it=i}})::rest ->
    used_locals (Set32.add i set) rest
  | {it=Block(_, body)|Loop(_, body)}::rest  ->
    used_locals (used_locals set body) rest
  | {it=If(_, body1, body2)}::rest ->
    used_locals (used_locals (used_locals set body1) body2) rest
  | _::rest -> used_locals set rest

(* Separately count how many swap variables were still used *)
let rec count_remaining_swaps num_args used = function
  | 0 -> 0
  | n -> if Set32.mem (Int32.of_int (num_args + n - 1)) used
    then 1 + (count_remaining_swaps num_args used (n-1))
    else count_remaining_swaps num_args used (n-1)

(* This will invalidate all liveness information for remapped locals.
   Not tracked since it will be recalculated anyway on the next pass. *)
let map_remaining_locals (types : Wasm.Ast.type_ list) {ftype; locals; num_swaps; body} =
  (* Must not modify locals which are function arguments *)
  let num_args = match List.nth types (Int32.to_int ftype.it) with
    {it=Wasm.Types.FuncType (args, _)} -> List.length args in
  let used = used_locals (Set32.of_list (List.init num_args Int32.of_int)) body in
  let new_swaps = count_remaining_swaps num_args used num_swaps in
  (* Set.elements is guarenteed to return elements in sorted order *)
  let mapping = List.mapi (fun i x -> (x, Int32.of_int i)) (Set32.elements used) in
  let rec map instr = {instr with it = match instr.it with
    | LocalGet ({it=i} as var) -> LocalGet{var with it = List.assoc i mapping}
    | LocalSet ({it=i} as var) -> LocalSet{var with it = List.assoc i mapping}
    | LocalTee ({it=i} as var) -> LocalTee{var with it = List.assoc i mapping}
    | Block(typ, body) -> Block(typ, List.map map body)
    | Loop(typ, body) -> Loop(typ, List.map map body)
    | If(typ, body1, body2) -> If(typ, List.map map body1, List.map map body2)
    | x -> x
    } in
  (* types of locals filtered to keep the ones that have mappings i.e. just remove the unused ones *)
  {ftype; locals=List.filteri (fun i _ -> List.mem_assoc (Int32.of_int i) mapping) locals;
   num_swaps=new_swaps; body=List.map map body}

let optimise ({funcs; types} as module_) =
  List.iter (fun {body} -> analyse_liveness (List.rev body)) funcs;
  let new_funcs = List.map (fun ({body} as f) -> {f with body=optimise_instrs body}) funcs in
  {module_ with funcs=List.map (map_remaining_locals types) new_funcs}
