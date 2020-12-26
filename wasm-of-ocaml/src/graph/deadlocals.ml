open Graph

(* TODO: Add unit tests for these functions! *)
let liveness_changing = ref false

let get_live_at_succ instr = List.fold_right (fun {live} set -> Set32.union (!live) set) (!(instr.succ)) Set32.empty

(* Steps in reverse instruction order, rather than directly traversing using pred/succ nodes.
   Could speed up by precomputing over basic blocks (would need new structure to identify/tack such blocks *)
(* Live = union of live at each successor + get - set/tee *)
let rec analyse_instr = function
  | {it=LocalGet {it=i}} as instr ->
    let new_live =  Set32.add i (get_live_at_succ instr) in
    if new_live = (!(instr.live)) then () else (liveness_changing := true; instr.live := new_live)
  | {it=(LocalTee {it=i} | LocalSet {it=i})} as instr  ->
    let new_live = Set32.remove i (get_live_at_succ instr) in
    if new_live = (!(instr.live)) then () else (liveness_changing := true; instr.live := new_live)
  | {it=(Block(_, body)|Loop(_, body))} ->
    List.iter analyse_instr body
  | {it=If(_, body1, body2)} ->
    List.iter analyse_instr body1; List.iter analyse_instr body2
  | instr ->
    let new_live = get_live_at_succ instr in
    if new_live = (!(instr.live)) then () else (liveness_changing := true; instr.live := new_live)

let rec analyse_liveness rev_body =
  List.iter analyse_instr rev_body;
  if (!liveness_changing) then ( liveness_changing := false; analyse_liveness rev_body)

(* Needed for keeping graph intact if a Tee instruction is removed.
   Need to check instructions either side are not already a possible successor/predecessor.
   e.g. e1; If({}; tee); If(e2; e3).
   In this case, e2 and e3 are already the successors of e1 and vice-versa, so actually can just remove tee
   from everywhere. *)
let remove_instr instr =
  let add_instr instrlst instr =
    if List.exists (fun i -> instr_eq i instr) (!instrlst) then ()
    else instrlst := instr::(!instrlst) in
  let s = !(instr.succ) and p = !(instr.pred) in
  (* Update the predecessors *)
  List.iter (fun i -> i.succ := List.filter (fun j -> not(instr_eq i instr)) (!(i.succ));
    List.iter (add_instr i.succ) s) p;
  (* Update the successors *)
  List.iter (fun i -> i.pred := List.filter (fun j -> not(instr_eq i instr)) (!(i.pred));
      List.iter (add_instr i.pred) p) s

(* No need to reset liveness analysis. Any which aren't valid on next pass will be removed, while rest remain correct *)
let rec optimise_instrs = function
  (* Look at successors, not that actual node (else would never be live) *)
  | ({it=LocalTee {it=i}} as instr)::rest ->
    if Set32.mem i (get_live_at_succ instr) then instr::(optimise_instrs rest)
    else (remove_instr instr; optimise_instrs rest)
  | ({it=LocalSet {it=i}} as instr)::rest ->
    if Set32.mem i (get_live_at_succ instr) then instr::(optimise_instrs rest)
    else ({instr with it=Drop})::(optimise_instrs rest)
  | ({it=Block(typ, body)} as instr)::rest ->
    (* Unnecessarily recreates body if no changes *)
    {instr with it=Block(typ, optimise_instrs body)}::(optimise_instrs rest)
  | ({it=Loop(typ, body)} as instr)::rest ->
    {instr with it=Loop(typ, optimise_instrs body)}::(optimise_instrs rest)
  | ({it=If(typ, body1, body2)} as instr)::rest ->
    {instr with it=If(typ, optimise_instrs body1, optimise_instrs body2)}::(optimise_instrs rest)
  | instr::instrs -> instr::(optimise_instrs instrs)
  | [] -> []

let optimise ({funcs} as module_) =
  List.iter (fun {body} -> analyse_liveness (List.rev body)) funcs;
  {module_ with funcs=List.map (fun ({body} as f) -> {f with body=optimise_instrs body}) funcs}
