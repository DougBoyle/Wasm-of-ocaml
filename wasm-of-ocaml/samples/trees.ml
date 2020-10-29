type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec completeBinaryTree x = function
  | 0 -> Leaf
  | n -> Node(x, 
              completeBinaryTree x (n-1), 
			  completeBinaryTree x (n-1))
			  
let rec countNodes = function
  | Leaf -> 1
  | Node(_, l, r) -> 1 + (countNodes l) + (countNodes r)
