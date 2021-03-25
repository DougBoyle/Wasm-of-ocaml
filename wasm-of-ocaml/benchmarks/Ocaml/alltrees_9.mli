type tree = Leaf | Node of tree * tree

val all_trees : int -> tree list

val a : tree list