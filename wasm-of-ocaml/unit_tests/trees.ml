(* Modified version of alltrees from benchmarks to pick up on mark/sweep garbage collection bugs *)
type tree = Leaf | Node of tree * tree

(* If map removed, instead get a memory access error (suggesting something was wrongly overwritten) *)
let rec map f = function
  | [] -> []
  | x::xs -> (f x)::(map f xs)

(* Error happens even if append written manually, so that isn't the issue
let rec append l1 l2 = match l1 with
  | [] -> l2
  | x::xs -> x::(append xs l2)
*)

let rec all_trees = function
  | 0 -> [Leaf]
  | n ->
  let rec trees_i i =
    let left_side = all_trees i
    and right_side = all_trees (n-1-i) in
    left_side @ right_side in
  let rec getall = function
    | 0 -> trees_i 0
    | n -> (trees_i n) @ (getall (n-1)) in
  getall (n-1)

let _ = all_trees 7 (* Works for 6, not for 7 *)