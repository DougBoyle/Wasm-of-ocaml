(* Functions for modifying graphs while maintaining the correct connectivity. Used by optimisations *)
open Graph

(* Update the successors/predecessors of a node to point to each other, so that instruction can be removed *)
let remove_instr instr =
  let add_instr instrlst instr =
    if List.exists (fun i -> instr_eq i instr) (!instrlst) then ()
    else instrlst := instr::(!instrlst) in
  let s = !(instr.succ) and p = !(instr.pred) in
  (* Update the predecessors *)
  List.iter (fun i -> i.succ := List.filter (fun j -> not(instr_eq j instr)) (!(i.succ));
    List.iter (add_instr i.succ) s) p;
  (* Update the successors *)
  List.iter (fun i -> i.pred := List.filter (fun j -> not(instr_eq j instr)) (!(i.pred));
      List.iter (add_instr i.pred) p) s

(*
  Insert action immediately after the original instruction in the graph.
  Pred/Succ are generated based on original and its successors, action is given a new ID so it remains unique.
  Returns the newly generated instruction
*)
let add_after original action =
  (* Note that the succ field must be copied, as it is about to be overwritten *)
  let new_instr = {pred = ref [original]; it = action; succ = ref (!(original.succ));
    live = ref Graph.Set32.empty; Graph.id = Graph.get_id ();} in
  (* Update the successors of the original instruction that they have a new successor *)
  List.iter (fun next_instr ->
    next_instr.pred :=  new_instr::(List.filter (fun i -> not(instr_eq i original)) (!(next_instr.pred))) )
    (!(original.succ));
  original.succ := [new_instr];
  new_instr

(* Used for inserting a decrement call before a Drop instruction when removing unused LocalSet instructions.
   As above just with pred/succ switched *)
let add_before original action =
  let new_instr = {succ = ref [original]; it = action; pred = ref (!(original.pred));
    live = ref Graph.Set32.empty; Graph.id = Graph.get_id ();} in
  (* Update the successors of the original instruction that they have a new successor *)
  List.iter (fun next_instr ->
    next_instr.succ :=  new_instr::(List.filter (fun i -> not(instr_eq i original)) (!(next_instr.succ))) )
    (!(original.pred));
  original.pred := [new_instr];
  new_instr

(* For replacing a Set/Get with a Tee, keep the first instruction and update instructions which
  point back to the second instruction. Actual instruction used is created by caller *)
let merge_instrs first second =
  List.iter
  (fun instr ->
    instr.pred := first::(List.filter (fun i -> not(instr_eq i second)) (!(instr.pred))))
  (!(second.succ))

(* Needed in several places and slightly awkward to calculate *)
let func_arity (types : Wasm.Ast.type_ list) fidx =
  match List.nth types (Int32.to_int fidx) with
    {it=Wasm.Types.FuncType (args, _)} -> List.length args
