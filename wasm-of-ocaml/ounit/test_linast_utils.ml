open OUnit2
open Linast
open LinastUtils

let f_arg = Ident.create_local "x"

(* Unlikely to catch issues with simple cases, so just give a couple of short problematic examples *)
(* let f x = x + y in f x -- free variables = [x; y] *)
let test_fv_x_y_free _ =
  let f = Ident.create_local "f" and y = Ident.create_local "y" and x = Ident.create_local "x" in
  let sample_program =
    LinastExpr.mklet f Global
    (Compound.mkfun [f_arg] (LinastExpr.compound (Compound.binary Add (Imm.id f_arg) (Imm.id y))))
    (LinastExpr.compound (Compound.app (Imm.id f) [Imm.id x])) in
  let free_in_prog = Ident.Set.of_list [x; y] in
  (* See OUnit docs for lots of customisation options to display the difference in values if failure *)
  assert_equal ~cmp:Ident.Set.equal ~msg:"Free variables don't match"
    free_in_prog (free_vars Ident.Set.empty sample_program)

(* let f x = x in f y -- free variables = [y] *)
let test_fv_y_free _ =
  let f = Ident.create_local "f" and y = Ident.create_local "y" in
  let sample_program =
    LinastExpr.mklet f Global
    (Compound.mkfun [f_arg] (LinastExpr.compound (Compound.imm (Imm.id f_arg))))
    (LinastExpr.compound (Compound.app (Imm.id f) [Imm.id y])) in
  let free_in_prog = Ident.Set.of_list [y] in
  (* See OUnit docs for lots of customisation options to display the difference in values if failure *)
  assert_equal ~cmp:Ident.Set.equal ~msg:"Free variables don't match"
    free_in_prog (free_vars Ident.Set.empty sample_program)

(* 2 locals needed for a FOR loop, 1 needed for each Let expression (function arguments done separately) *)
(*
Needs 3 locals, maximum needed for body of x (f, start, end) and at final expression (f, x, z)
let f x = x in
  let x = for y = 0 to 0 do () done in
    let z = while false do () done in
      z
*)
let test_3_locals_needed _ =
   let f = Ident.create_local "f"  and x = Ident.create_local "x"
   and y = Ident.create_local "y" and z = Ident.create_local "z" in
   let zero_imm = Imm.const (Asttypes.Const_int 0) in
   let sample_program =
   (* Easier to declare as a list of binds *)
   binds_to_anf
   [BLet(f, Compound.mkfun [f_arg] (LinastExpr.compound (Compound.imm (Imm.id f_arg))));
    BLet(x, Compound.mkfor y zero_imm zero_imm Upto
      (LinastExpr.compound (Compound.imm zero_imm)));
    BLet(z, Compound.mkwhile (LinastExpr.compound (Compound.imm zero_imm))
      (LinastExpr.compound (Compound.imm zero_imm)));]
   (LinastExpr.compound (Compound.imm (Imm.id z))) in
   assert_equal 3 (count_vars sample_program)

(*
With for loop moved down, need 4 locals, maximum needed for body of x (f, z, start, end)
let f x = x in
  let z = while false do () done in
    let x = for y = 0 to 0 do () done in
      z
*)
let test_4_locals_needed _ =
   let f = Ident.create_local "f"  and x = Ident.create_local "x"
   and y = Ident.create_local "y" and z = Ident.create_local "z" in
   let zero_imm = Imm.const (Asttypes.Const_int 0) in
   let sample_program =
   (* Easier to declare as a list of binds *)
   binds_to_anf
   [BLet(f, Compound.mkfun [f_arg] (LinastExpr.compound (Compound.imm (Imm.id f_arg))));
    BLet(z, Compound.mkwhile (LinastExpr.compound (Compound.imm zero_imm))
      (LinastExpr.compound (Compound.imm zero_imm)));
    BLet(x, Compound.mkfor y zero_imm zero_imm Upto
      (LinastExpr.compound (Compound.imm zero_imm)));]
   (LinastExpr.compound (Compound.imm (Imm.id z))) in
   assert_equal 4 (count_vars sample_program)

let suite =
  "Testing free variables calculation" >::: [
    "test x and y free" >:: test_fv_x_y_free;
    "test y free" >:: test_fv_y_free;
    "test 3 locals needed" >:: test_3_locals_needed;
    "test 4 locals needed" >:: test_4_locals_needed
  ]

let () =
  run_test_tt_main suite