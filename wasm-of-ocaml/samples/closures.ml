(*
   Globals shouldn't be put into closures.
   (all other optimisations should be turned off to test this, prevents constant propagation)
 *)
let a = 5
let b = 10
let f x = b + a
let a = 3 (* Prevents the 'a' in 'f' from being exported. Still not needed in closure *)

(* But shouldn't generate an unnecessary number of globals for top-level temporaries either *)
let x = 1 + 2 + 3 + 4 + 5
