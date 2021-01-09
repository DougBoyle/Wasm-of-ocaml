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
and e x = if x = 0 then f x else e (x-1)

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

(*
Old version of tail recursion analysis:
p : optimised on its own

f : both rewritten to be mutually recursive (turn into 4 functions),
e : despite f only ever calling itself.

g : rewritten, but the wrapper function is removed due to not being used.
h : rewritten. Not particularly useful since f only makes non-tail calls to either of them
    so stack will still increase with each loop to f, just increases slightly slower.

isEven : both actually mutually recursive
isOdd :


Version 2 of tail recursion analysis:
p : p       (tail call optimised on its own)

f : f       (f is tail reucrsive on its own, so it will be done separately.
e : f e      e is then also only making tail calls to itself so also done separately.
             The tail call from e to f doesn't have much benefit optimising, as it happens at most once. )

g :         (g does make a tail call to f, but f is not defined in the same letrec group.
h : g        Therefore, neither of these will be rewritten. )

isEven : isOdd      (both really are mutually tail recursive so optimised as such)
isOdd : isEven

*)

