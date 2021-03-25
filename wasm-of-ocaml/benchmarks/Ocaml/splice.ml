let rec splice = function
  | [], ly -> ly
  | lx, [] -> lx
  | x::xs, y::ys -> x::y::(splice (xs, ys))

(* Generate a list of 0s to test with *)
let rec init n =
  if n = 0 then [] else 0::(init (n-1))

let n = 1000
let m = 1000
let a =
  let l = ref [] in
  let l1 = init n and l2 = init n in
  for i = 1 to m do l := splice (l1, l2) done;
  !l

