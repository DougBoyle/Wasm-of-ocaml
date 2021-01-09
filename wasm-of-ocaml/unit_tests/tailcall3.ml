(* Same as above but unnecessarily making each of f and g mutually recursive, to check the other optimisation case *)

let rec f x =
  let rec g x = f (x - 1)
  and h x = g x in
  if x = 0 then x else
    let a = h x in
    f a
and e x = if x = 0 then f x else e (x-1)

let rec p x =
  let rec r x = if x = 0 then p (x - 1) else s (x-1)
  and s x = r x in
  if x = 0 then x else
  if x = 1 then q x else
    let a = s x in
    p a
and q x = if x = 0 then p x else q (x-1)

(*
-- Version 1:
f and e are both made mutually tail recursive. This introduces an extra function for each of them
  which only save the function call from e to f, and adds indirection to each function.
g and h are also both made mutually recursive due to the call to (now mutually recursive) f.
  As the call to a = h x cannot be replaced, this means there is only 1 extra stack frame rather than
  3 by the time f (x-1) is called. But it introduces more complexity and splits both into 2 functions
  (although the wrapper for g can be dead-assign eliminated as the only calls to it are tail calls)
p, q, r, s do exactly the same, as p and q are mutually recursive again.

Negatives: f and e made mutually recursive for minimal benefit.

-- Version 2:
f and e are optimised on their own.
g and h are not optimised.
p and q are made mutually recursive.
s and r are not optimised as they are only mutually recursive with p, which is in another letrec block.

Negatives: s and r not made mutually recursive. Still only able to reduce number of frames from
  3 per loop to 1 per loop, and added complexity of making mutually recursive may not be worth it.

-- Version 3: Anywhere an unlimited number of tail calls can be removed entirely, they are.
If only a fraction of calls can be removed, we choose not to rewrite since it
increases code size. (e.g. optimisation that would change every 3 stack cells to just 1 isn't done)

f and e are optimised on their own, since f only makes tail calls to itself
so the tail call to f from e increases the stack at most once.
As f only tail calls itself so should not be mutually tail recursive, neither are g or h.

p q r s are all optimised together, since p and q each makes recursive calls to both p and q so they
should be mutually tail recursive. Since p is also tail called by r, and r is tail called by s,
these also benefit from being made mutually tail recursive.

Negatives: opposite of above. Unclear if s and r should be mutually recursive or not.

*)

