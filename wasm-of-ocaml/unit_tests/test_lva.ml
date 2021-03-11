(* Simple test case of live variable analysis, to check analysis has no obvious errors *)

(* Annotated with expected live variables before each instruction executes i.e. in-live

  l1 = l0       - [0, 2, 4]
  l2            - [1, 2, 4]
  Tee l3        - [1, 2, 4]

  if {
    l2 = 5      - [4]
  } else {
    l3 = l1     - [1, 2]
    l4 = l3     - [2, 3]
  }

  l1 = l2 + l4  - [2, 4]
*)

open OUnit2
open Graph
open Compilewasm
open Bindstree

let num_swaps = Int32.of_int (List.length swap_slots)

let compile_get i =
  LocalGet (add_dummy_loc i)
let compile_tee i =
  LocalTee (add_dummy_loc i)
let compile_set i =
  LocalSet (add_dummy_loc i)

let create_program _ =
  (* Instructions to test liveness of *)
  let a = add_dummy_edges (compile_get 0l)
  and b = add_dummy_edges (compile_get 2l)
  and c = add_dummy_edges (compile_tee 3l)
  and d = add_dummy_edges (Const(encoded_const_int 5))
  and e = add_dummy_edges (compile_get 1l)
  and f = add_dummy_edges (compile_get 3l)
  and g = add_dummy_edges (compile_get 2l) in

  let program =
  [a; add_dummy_edges (compile_set 1l);
   b;
   c;
   add_dummy_edges (If(ValBlockType None,
     [d; add_dummy_edges (compile_set 2l)],
     [e; add_dummy_edges (compile_set 3l);
      f; add_dummy_edges (compile_set 4l)]
   ));
   g;
   add_dummy_edges (compile_get 4l);
   add_dummy_edges (Binary(Wasm.Values.I32 Wasm.Ast.IntOp.Add));
   add_dummy_edges (compile_set 1l);
  ] in
  (* Connects the edges of the graph. process_instrs called rather than passing a whole module *)
  process_instrs [] [] program;
  (a,b,c,d,e,f,g), program

let is_live i instr = Set32.mem i (!(instr.live))
let is_dead i instr = not (is_live i instr)

let test_lva _ =
  let (a,b,c,d,e,f,g), program = create_program () in
  Deadlocals.analyse_liveness (List.rev program);
  (* Uncomment this as quickest way to verify whole program correctly analysed *)
  (* Ppgraph.print_block Format.std_formatter program; *)
  assert_bool "Variables not live where they should be"
    (List.for_all (is_live 0l) [a] &&
     List.for_all (is_live 1l) [b; c; e] &&
     List.for_all (is_live 2l) [a; b; c; e; f; g] &&
     List.for_all (is_live 3l) [f] &&
     List.for_all (is_live 4l) [a; b; c; d; g]);
  assert_bool "Variables live when should not be"
    (List.for_all (is_dead 0l) [b; c; d; e; f; g] &&
     List.for_all (is_dead 1l) [a; d; f; g] &&
     List.for_all (is_dead 2l) [d] &&
     List.for_all (is_dead 3l) [a; b; c; d; e; g] &&
     List.for_all (is_dead 4l) [e; f])

let suite =
  "Testing lva analysis with if branch" >::: [
    "test correct liveness before each instruction" >:: test_lva;
  ]

let () =
  run_test_tt_main suite
