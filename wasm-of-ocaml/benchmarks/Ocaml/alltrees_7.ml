type tree = Leaf | Node of tree * tree

let rec map f = function
  | [] -> []
  | x::xs -> (f x)::(map f xs)
let rec foldr f l e = match l with
  | [] -> e
  | x::xs -> f x (foldr f xs e)

let rec all_trees = function
  | 0 -> [Leaf]
  | n ->
  let rec trees_i i =
    let left_side = all_trees i
    and right_side = all_trees (n-1-i) in
    foldr (fun left trees -> (map (fun right -> Node(left, right)) right_side) @ trees) left_side [] in
  let rec getall = function
    | 0 -> trees_i 0
    | n -> (trees_i n)@(getall (n-1)) in
  getall (n-1)

let a = all_trees 7