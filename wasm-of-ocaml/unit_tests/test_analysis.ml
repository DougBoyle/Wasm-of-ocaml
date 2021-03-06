(*
  Set of test programs to check that the Linast analysis correctly identifies terms
  that are pure (no side effects) and immutable (output is always the same)

  Pure expressions can be dead assignment eliminated (nothing achieved by calculating and discarding)
  Immutable expressions can be common subexpression eliminated (as above, and result is also the same, can reuse)
*)
open OUnit2
open LinastUtils
open Linast

let block_id = Ident.create_local "id"

(* The function call to f means that 1) f could perform effects so result is impure and
   2) Fields of the block may have been modified, so the result may not be 0 *)
(* TODO: Add analysis for mutable fields by tracking a list of (known value) blocks.
     i.e. something gets removed from the list as soon as it is passed to a function or has fields set,
     or put into another block. Probably also remove if assigned to another variable, rather than trying
     to remember all of the variable names that invalidate a given piece of information. *)
let test_mutable_field_of_block_with_function _ =
  let program =
    LinastExpr.mklet block_id Local
      (Compound.makeblock ~annotations:(ref [ImmutableBlock [false]]) 0 [Imm.const (Asttypes.Const_int 0)])
      (LinastExpr.seq (Compound.app (Imm.id (Ident.create_local "f")) [Imm.id block_id])
      (LinastExpr.compound (Compound.field (Imm.id block_id) 0))) in
  AnalysePurity.analyse program;
  assert_bool "Program is marked as pure" (not (List.mem Pure (!(program.annotations))));
  assert_bool "Program is marked as immutable" (not (List.mem Immutable (!(program.annotations))))

(* Nothing is known about block_id, so fields are assumed to be mutable *)
let test_mutable_field_of_block _ =
  let program = LinastExpr.compound (Compound.field (Imm.id block_id) 0) in
  AnalysePurity.analyse program;
  assert_bool "Program is not marked as pure" (List.mem Pure (!(program.annotations)));
  assert_bool "Program is marked as immutable" (not (List.mem Immutable (!(program.annotations))))

let test_immutable_field_of_block_with_function _ =
  let program =
    LinastExpr.mklet block_id Local
      (Compound.makeblock ~annotations:(ref [ImmutableBlock [true]]) 0 [Imm.const (Asttypes.Const_int 0)])
      (LinastExpr.seq (Compound.app (Imm.id (Ident.create_local "f")) [Imm.id block_id])
      (LinastExpr.compound (Compound.field (Imm.id block_id) 0))) in
  AnalysePurity.analyse program;
  assert_bool "Program is marked as pure" (not (List.mem Pure (!(program.annotations))));
  assert_bool "Program is marked as immutable" (not (List.mem Immutable (!(program.annotations))))

let test_immutable_field_of_block _ =
  let program =
    LinastExpr.mklet block_id Local
      (Compound.makeblock ~annotations:(ref [ImmutableBlock [true]]) 0 [Imm.const (Asttypes.Const_int 0)])
      (LinastExpr.compound (Compound.field (Imm.id block_id) 0)) in
  AnalysePurity.analyse program;
  assert_bool "Program is not marked as pure" (List.mem Pure (!(program.annotations)));
  assert_bool "Program is not marked as immutable" (List.mem Immutable (!(program.annotations)))

let suite =
  "IR analysis and optimisation" >::: [
    "mutable field access is not pure when function called" >:: test_mutable_field_of_block_with_function;
    "mutable field access is only pure" >:: test_mutable_field_of_block;
    "immutable field access is not pure when function called" >:: test_immutable_field_of_block_with_function;
    "immutable field access on its own is immutable" >:: test_immutable_field_of_block;
  ]

let () =
  run_test_tt_main suite
