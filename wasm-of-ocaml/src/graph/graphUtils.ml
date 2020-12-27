(* Functions for modifying graphs while maintaining the correct connectivity. Used by optimisations *)
open Graph

(* Update the successors/predecessors of a node to point to each other, so that instruction can be removed *)
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

(*
  Insert action immediately after the original instruction in the graph.
  Pred/Succ are generated based on original and its successors, action is given a new ID so it remains unique.
  Returns the newly generated instruction
*)
let add_after original action =
  let new_instr = {pred = ref [original]; it = action; succ = original.succ;
    live = ref Graph.Set32.empty; Graph.id = Graph.get_id ();} in
  original.succ := [new_instr];
  new_instr

(* For replacing a Set/Get with a Tee, keep the first instruction and update instructions which
  point back to the second instruction. Actual instruction used is created by caller *)
let merge_instrs first second =
  List.iter
  (fun instr ->
    instr.pred := first::(List.filter (fun i -> not(instr_eq i second)) (!(instr.pred))))
  (!(second.succ));
