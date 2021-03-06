(* Identical to match.ml but with functions *)
let f1 = function
  | 0 -> 0
  | 1 -> 0
  | _ -> 1
let f2 = function
  | (2, _) -> 0
  | (_, 1) -> 0
  | (_, 2) -> 1
  | _ -> 0
let f3 = function
  | [||] -> 0
  | [|2|] -> 0
  | [|_|] -> 1
  | [|_; _|] -> 0
  | _ -> 0

type t = {x : int; y : int}
let f4 = function
  | {x = 2} -> 0
  | {y = 1} -> 0
  | _ -> 1

type lst = Nil | Cons of int * lst
let f5 = function
  | Nil -> 0
  | Cons(_, Cons(_, _)) -> 0
  | Cons(1, _) -> 1
  | Cons(_, _) -> 0

let a = f1 2
let b = f2 (1, 2)
let c = f3 [|1|]
let d = f4 {x = 1; y = 2}
let e = f5 (Cons(1, Nil))