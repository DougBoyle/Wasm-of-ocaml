(* After optimisation passes complete, some annotations may no longer be valid.
   Filter the annotations on each term to just those that should be preserved. *)
(*
Annotations left unchanged:
  ImmutableBlock - property of the term that block came from i.e. tuple/record/array

All other annotations will be added again when optimisation pass repeated, so don't preserve them.
*)
open Linast

let clear_annotations annotations =
  annotations := List.filter
    (function ImmutableBlock _ | TailCallOptimised | Tupled -> true | _ -> false) (!annotations)

let map_imm (imm : imm_expr) =
  clear_annotations imm.i_annotations; imm
let enter_compound (compound : compound_expr) =
  clear_annotations compound.annotations; compound
let enter_linast linast =
  clear_annotations linast.annotations; linast

let clear linast =
  (LinastMap.create_mapper ~enter_linast ~enter_compound ~map_imm ()) linast
