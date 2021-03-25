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

