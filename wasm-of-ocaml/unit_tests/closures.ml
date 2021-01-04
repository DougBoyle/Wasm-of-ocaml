(*
   Globals shouldn't be put into closures.
   (optimisations should be turned off to test this, prevents constant propagation)
 *)
let a = 5
let f x = x + a
