let a = match 2 with
  | 0 -> 0
  | 1 -> 0
  | _ -> 1
let b = match (1,2) with
  | (2, _) -> 0
  | (_, 1) -> 0
  | (_, 2) -> 1
  | _ -> 0
let c = match [|1|] with
  | [||] -> 0
  | [|2|] -> 0
  | [|_|] -> 1
  | [|_; _|] -> 0
  | _ -> 0
type t = {x : int; y : int}
let d = match {x = 1; y = 2} with
  | {x = 2} -> 0
  | {y = 1} -> 0
  | _ -> 1
type lst = Nil | Cons of int * lst
let e = match Cons(1, Nil) with
  | Nil -> 0
  | Cons(_, Cons(_, _)) -> 0
  | Cons(1, _) -> 1
  | Cons(_, _) -> 0
