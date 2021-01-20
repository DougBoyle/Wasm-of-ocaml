(*
  Tests a fairly simple case of branching control flow, to check that
  nodes are correctly linked together and that adding/removing nodes
  maintains the structure of the tree
 *)

(*
  const 0     - A
  If {
    const 0   - B
  } else {
    const 0   - C
    br_if     - D
    const 0   - E
  }
  const 0     - F
*)

(*
 A -> B, C
 B -> F
 C -> D
 D -> E, F
 E -> F
*)

open OUnit2
open Graph
open Compilewasm

let create_program _ =
  let a = add_dummy_edges (Const(const_int32 0))
  and b = add_dummy_edges (Const(const_int32 0))
  and c = add_dummy_edges (Const(const_int32 0))
  and d = add_dummy_edges (Graph.BrIf (add_dummy_loc (Int32.of_int 0)))
  and e = add_dummy_edges (Const(const_int32 0))
  and f = add_dummy_edges (Const(const_int32 0)) in
  (* Connects the edges of the graph. process_instrs called rather than passing a whole module *)
  let program = process_instrs [] []
    [a; add_dummy_edges (Graph.If(ValBlockType None, [b], [c;d;e])); f] in
  (a, b, c, d, e, f), program

let contains_instr set instr =
  (List.find_opt (fun i -> i.id = instr.id) (!set)) <> None

let is_linked i j =
  contains_instr i.succ j && contains_instr j.pred i

(* TODO: Come up with better error message *)
let test_initial_graph _ =
  let (a, b, c, d, e, f), program = create_program () in
  List.iter (fun (i, j) ->
    assert_bool "Instruction A not followed by B" (is_linked i j))
    [(a, b); (a, c); (b, f); (c, d); (d, e); (d, f); (e, f)]

(* Remove and insert are both only used in 'simple' situations, optimisations never try to change the
   graph in a complicated way (e.g. inserting an extra branch or removing one).
   For these, would just clear and reconstruct the edges of the graph rather than trying to track it *)
let test_remove _ =
  let (a, b, c, d, e, f), program = create_program () in
  GraphUtils.remove_instr c;
  List.iter (fun (i, j) ->
    assert_bool "Instruction A not followed by B" (is_linked i j))
    [(a, b); (a, d); (b, f); (d, e); (d, f); (e, f)];
  (* Also check that A and D are no longer linked to C.
     This is only checked one way round, as we don't care what state C was left in *)
  assert_bool "Instructions still linked to C"
    (not (contains_instr a.succ c) && not (contains_instr d.pred c))

let test_insert _ =
  let (a, b, c, d, e, f), program = create_program () in
  let new_instr = GraphUtils.add_after a (Const(const_int32 0)) in
  List.iter (fun (i, j) ->
    assert_bool "Instruction A not followed by B" (is_linked i j))
    [(a, new_instr); (new_instr, b); (new_instr, c); (b, f); (c,d); (d, e); (d, f); (e, f)];
  (* Also check that B and C are no longer linked to A *)
  assert_bool "Instructions still linked to A"
    (not (contains_instr a.succ b) && not (contains_instr b.pred a) &&
     not (contains_instr a.succ c) && not (contains_instr c.pred a))

let suite =
  "Testing graph construction and modification" >::: [
    "test graph connected" >:: test_initial_graph;
    "successfully disconnect a removed instruction" >:: test_remove;
    "successfully insert a new instruction" >:: test_insert;
  ]

let () =
  run_test_tt_main suite

