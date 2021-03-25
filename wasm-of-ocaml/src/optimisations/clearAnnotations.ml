(* After optimisation passes complete, some annotations may no longer be valid.
   Filter the annotations on each term to just those that should be preserved. *)
open Linast

let clear_annotations annotations =
  annotations := List.filter
    (function ImmutableBlock _ | TailCallOptimised | Tupled -> true | _ -> false) (!annotations)

let map_imm imm =
  clear_annotations imm.i_annotations; imm
let enter_compound compound =
  clear_annotations compound.c_annotations; compound
let enter_linast linast =
  clear_annotations linast.annotations; linast

let clear linast =
  (LinastMap.create_mapper ~enter_linast ~enter_compound ~map_imm ()) linast
