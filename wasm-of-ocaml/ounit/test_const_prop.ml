(*
  Check that constant propagation of values in memory only makes optimisations where safe to do so.
  TODO: Add some analysis of guarenteed mutable fields
*)
open OUnit2
open LinastUtils
open Linast

let block_id = Ident.create_local "id"

let rec ends_in_a_constant linast = match linast.desc with
  | LLet(_, _, _, body) | LLetRec(_, body) | LSeq(_, body) -> ends_in_a_constant body
  | LCompound {desc=CImm _} -> true
  | _ -> false

(* Function call potentially modifies the field, so can't optimise *)
let test_mutable_field_of_block_with_function _ =
  let program =
    LinastExpr.mklet block_id Local
      (Compound.makeblock ~annotations:(ref [ImmutableBlock [false]]) 0 [Imm.const (Asttypes.Const_int 0)])
      (LinastExpr.seq (Compound.app (Imm.id (Ident.create_local "f")) [Imm.id block_id])
      (LinastExpr.compound (Compound.field (Imm.id block_id) 0))) in
  AnalysePurity.analyse program;
  AnalyseBlocks.analyse program;
  assert_bool "Program wrongly optimised" (not (ends_in_a_constant (OptConstants.optimise program)))

(* Nothing is known about block_id, so fields are assumed to be mutable and can't be optimised *)
let test_mutable_field_of_block _ =
  let program = LinastExpr.compound (Compound.field (Imm.id block_id) 0) in
  AnalysePurity.analyse program;
  AnalyseBlocks.analyse program;
  assert_bool "Program wrongly optimised" (not (ends_in_a_constant (OptConstants.optimise program)))

(* Function call makes expression impure, but the final result is still known *)
let test_immutable_field_of_block_with_function _ =
  let program =
    LinastExpr.mklet block_id Local
      (Compound.makeblock ~annotations:(ref [ImmutableBlock [true]]) 0 [Imm.const (Asttypes.Const_int 0)])
      (LinastExpr.seq (Compound.app (Imm.id (Ident.create_local "f")) [Imm.id block_id])
      (LinastExpr.compound (Compound.field (Imm.id block_id) 0))) in
  AnalysePurity.analyse program;
  AnalyseBlocks.analyse program;
  assert_bool "Program not optimised" (ends_in_a_constant (OptConstants.optimise program))

(* Trivial case where the field can be extracted *)
let test_immutable_field_of_block _ =
  let program =
    LinastExpr.mklet block_id Local
      (Compound.makeblock ~annotations:(ref [ImmutableBlock [true]]) 0 [Imm.const (Asttypes.Const_int 0)])
      (LinastExpr.compound (Compound.field (Imm.id block_id) 0)) in
  AnalysePurity.analyse program;
  AnalyseBlocks.analyse program;
  assert_bool "Program not optimised" (ends_in_a_constant (OptConstants.optimise program))

let suite =
  "IR analysis and optimisation" >::: [
    "mutable field access cannot be optimised" >:: test_mutable_field_of_block_with_function;
    "unknown mutable field access cannot be optimised" >:: test_mutable_field_of_block;
    "immutable field access can be optimised even if overall impure" >:: test_immutable_field_of_block_with_function;
    "immutable field access can be optimised" >:: test_immutable_field_of_block;
  ]

let () =
  run_test_tt_main suite
