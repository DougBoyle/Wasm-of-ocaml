(*
  Must run with IR optimisations but no inlining or dead assignment elimination
*)

(* ISSUE: fails to garbage collect [] if not written this way, since it will be incremented
   on the assumption that it is a local that will then be decremented when all locals are decremented *)
let rec f x =
  let a = [] in
  a
 (* [] *)

(* Move out of top-level so variables are locals, not globals *)
let main _ =
  let _ = f 0 in
  0

let b = main ()


