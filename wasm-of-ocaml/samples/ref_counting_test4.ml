(*
  Must run with all IR optimisations disabled

*)

(* ISSUE: can't always tell whether result should be incremented or not
  (at least not without better analysis). So have to increment in situations like this,
  which could end up leaking [].
  Is this solved by forcing functions to always return an immediate or not??
  No. x = if ... y else []   - we avoid incrementing on fresh allocations, but can't tell here either
  i.e. forced to leak memory in non-obvious cases

  Move 'return' within the if block so that they then can be handled deterministically.
     Currently allocates 3 objects at end, should only need 2.
*)
let rec g x = if false then x else []
(* let rec g x = [] *)

(* Move out of top-level so variables are locals, not globals *)
let rec main _ =
  let a = g [] in
  0

let b = main ()


