(*
  Tests a case which the Grain compiler fails on with optimisations enabled.
  f (x-1) is not a tail call, since g x is not defined as a recursive binding so expects an actual result.
  Were g x declared as recursive, it could indeed be tail call optimised and would work correctly (+ efficiently)
*)
(* Can be simplified to just 'f 0 = 0 | n = f (n-1)' *)
let rec p x =
  let l x = p (x - 1) in
  if x = 0 then x else
    let a = l x in
    p a

let a = p 5

(* Same as above but unnecessarily making each of f and g mutually recursive, to check the other optimisation case *)

let rec f x =
  let rec g x = f (x - 1)
  and h x = g x in
  if x = 0 then x else
    let a = h x in
    f a
and e x = f x

let b = f 5

(* Also test mutually recursive functions *)
(* isEven true n - returns if n is even
   isOdd false n - returns if n is odd *)
let rec isEven b n =
  if n = 0 then b else isOdd (not b) (n-1)
and isOdd b n =
  if n = 0 then b else isEven (not b) (n-1)

let c = isEven true 10
let d = isEven true 9
