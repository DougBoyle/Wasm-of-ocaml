(* Adaptation of the more complicated version mentioned in the paper *)
type lst = Nil | One of int | Cons of int * lst

let f l1 l2 = match (l1, l2) with
  | Nil, _ -> 0
  | _, Nil -> 1
  | ((One _, _)|(_, One _)) -> 2
  | Cons _, Cons _ -> 3

let n = 1000000
let a =
  let x = ref 0 in
  let l1 = Cons(1, Nil) and l2 = One 2 in
  for i = 1 to n do x := f Nil l2; x := f l1 Nil;  x := f l1 l2;  x := f l1 l1;   done;
  !x
