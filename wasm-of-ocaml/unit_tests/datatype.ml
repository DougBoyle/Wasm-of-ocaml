type ls = Nil | Cons of int * ls

let rec sum = function
  | Nil -> 0
  | Cons(x, tail) -> x + (sum tail)

let x = Cons(1, Cons(2, Cons(3, Nil)))
let y = sum x
