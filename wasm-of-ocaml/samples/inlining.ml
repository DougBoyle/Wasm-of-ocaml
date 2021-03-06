(*
Without inlining:
(let
  (fun/106 = (function x/89 y/90 (x/89 + y/90))
   export x/88 = (apply fun/106 4 5)
   fun/104 = (function y/92 x/93 (y/92 + x/93))
   export f/91 = (apply fun/104 x/88)
   export z/94 = (apply f/91 3)
   compound/99 = (function x/97 x/97)
   fun/100 = (function f/96 f/96)
   export a/95 = (apply fun/100 compound/99 5))
  0)

With inlining (of just directly applied functions):
(let
  (export x/88 = 9
   export f/91 = (function x/93 (9 + x/93))
   export z/94 = (apply f/91 3)
   export a/95 = 5)
  0)
*)

let x = (fun x y -> x + y) 4 5
let f = (fun y x -> y + x) x (* Under applied and possible substitution issue *)
let z = f 3
let a = (fun f -> f) (fun x -> x) 5 (* Over applied *)
