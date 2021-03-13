(* Adaptation of the more complicated version mentioned in the paper *)
type lst = Nil | One of int | Cons of int * lst

let rec splice l1 l2 = match (l1, l2) with
  | Nil, _ -> 0
  | _, Nil -> 0
  | ((One x, _)|(_, One x)) -> x
  | Cons (x, xs), Cons (y, ys) -> x + y

(* Generate a list of 0s to test with *)
let rec init n =
  if n = 0 then Nil else Cons(n, (init (n-1)))

let n = 1000
let m = 1000
let a =
  let x = ref 0 in
  let l1 = Cons(1, Nil) and l2 = One 2 in
  for i = 1 to m*n do x := splice l1 l2 done;
  !x
