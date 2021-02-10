(* Very memory intensive, would just counting cases be better? i.e. make recursion relation more explicit *)
(* Intentionally terrible recursive implementation i.e. not caching sub-results, using regular append.
   Estimate of minimum: 16000 trees, each at least 10 Nodes which are [tag, arity, t1, t2].
                        Cell = 4 bytes => minimum of 16k * 10 * 4 * 4 = 2.56 MB *)
type tree = Leaf | Node of tree * tree

val all_trees : int -> tree list

val a : tree list