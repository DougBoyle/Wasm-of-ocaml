(* First match can be used to check dead branch elimination (others require multiple passes to resolve branch)
Before:
(let
  (compound/119 =
     (try (switch 2 case tag 0: 0
                    case tag 1: 0
                    default: (fail 8))
      with (8) 1)
   export a/84 = compound/119
   ...

After:
(let
  (compound/119 = (try (fail 8) with (8) 1) --- Gets simplified by optFails.ml
   export a/84 = compound/119
  ...

After also simplifying unnecessary try/catch blocks:
(let
  (compound/119 = 1
   export a/84 = compound/119    --- Another round of constant propagation will simplify even further
  ...
*)
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
