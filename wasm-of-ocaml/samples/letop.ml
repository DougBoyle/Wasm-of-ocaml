type 'a t = unit -> 'a seq
and 'a seq = Nil | Cons of 'a * 'a t

let rec return x = fun () -> Cons(x, return x)

let rec prod a b = fun () -> match a (), b () with 
  | Nil, _ | _, Nil -> Nil
  | Cons(x, a), Cons(y, b) -> Cons((x, y), prod a b)

let rec map f s = fun () -> match s() with Nil -> Nil | Cons(x, a) -> Cons(f x, map f a)

let (let+) f s = map s f
let (and+) a b = prod a b

let sum3 z1 z2 z3 = let+ x1 = z2 and+ x2 = z2 and+ x3 = z3 in x1 + x2 + x3
