(*
  Must run with IR optimisations but no inlining or dead assignment elimination.

  Expected result:
  Leaves 3 objects allocated (current 24B each), but the [] created in main and passed to g can be gc'd
*)

(* ISSUE: as shown by test2, need to increment only local binds before cleaning up locals.
   Hence returning x or [] won't increment in either case, and caller won't increment result either.
   However, if g returns x, main returning g x is the same as x so should be incremented, whereas
   if g returns a fresh [] then main effectively returns [] which is not a local so should not be.
   Only way to distinguish the two is by g also incrementing the result when it is an arg bind,
   and decrementing its args. If g decrements all of its args, the caller also needs to increment them. *)
 let rec g x = x
(* let rec g x = [] *)

(* Move out of top-level so variables are locals, not globals *)
let rec main _ =
  let a = g [] in
  a

let b = main ()


