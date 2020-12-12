(* Translation to IR collapses down nested functions into curried syntax.
   This gets unrolled later but makes IR representation neater and may be helpful for optimisations.
   This test verifies that the process does not forget any bindings in the process so is safe to do. *)
let f (x, y) z = x + y + z
let g x (y, z) = x + y + z
let a = f (1, 2) 3
let b = g 1 (2, 3)
