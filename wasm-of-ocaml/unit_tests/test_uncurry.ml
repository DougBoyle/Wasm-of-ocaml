(* Check simple case of ensuring uncurrying only done when safe *)
open OUnit2
open LinastUtils
open Linast

let f_id = Ident.create_local "f"
let x = Ident.create_local "x"
let y = Ident.create_local "y"

let is_optimised linast = match linast.desc with
  | LLet(_, _, compound, _) ->
    List.mem Tupled (!(compound.c_annotations))
  | _ -> false

(* Only call is fully applied so can optimise *)
let test_only_fully_applied _ =
  let program =
    LinastExpr.mklet f_id Local
    (Compound.mkfun [x; y] (LinastExpr.compound (Compound.imm unit_value)))
    (LinastExpr.compound (Compound.app (Imm.id f_id) [unit_value; unit_value] )) in
  assert_bool "Function not optimised" (is_optimised (OptTuple.optimise program))

(* Under-applied, so can't optimise *)
let test_not_fully_applied _ =
  let program =
    LinastExpr.mklet f_id Local
      (Compound.mkfun [x; y] (LinastExpr.compound (Compound.imm unit_value)))
    (LinastExpr.seq (Compound.app (Imm.id f_id) [unit_value; unit_value])
    (LinastExpr.compound (Compound.app (Imm.id f_id) [unit_value]))) in
  assert_bool "Function wrongly optimised" (not (is_optimised (OptTuple.optimise program)))

(* Will optimise if over-applied (example doesn't actually return a function) *)
let test_only_over_applied _ =
  let program =
    LinastExpr.mklet f_id Local
    (Compound.mkfun [x; y] (LinastExpr.compound (Compound.imm unit_value)))
    (LinastExpr.compound (Compound.app (Imm.id f_id) [unit_value; unit_value; unit_value] )) in
  assert_bool "Function not optimised" (is_optimised (OptTuple.optimise program))

(* Don't optimise if function can be passed around as a value somewhere *)
let test_unknown_use _ =
  let g_id = Ident.create_local "g" in
  let program =
    LinastExpr.mklet f_id Local
    (Compound.mkfun [x; y] (LinastExpr.compound (Compound.imm unit_value)))
    (LinastExpr.mklet g_id Local (Compound.imm (Imm.id f_id))
      (LinastExpr.compound (Compound.app (Imm.id g_id) [unit_value]))) in
  assert_bool "Function wrongly optimised" (not (is_optimised (OptTuple.optimise program)))


let suite =
  "Uncurrying optimisation" >::: [
    "Can optimise function always fully applied" >:: test_only_fully_applied;
    "Can't optimise function under applied" >:: test_not_fully_applied;
    "Can optimise function always over applied" >:: test_only_over_applied;
    "Can't optimise function passed elsewhere as value" >:: test_unknown_use;
  ]

let () =
  run_test_tt_main suite
