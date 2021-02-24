(* See: https://www.hboehm.info/gc/gc_bench/GCBench.java
  Allocates and collects balanced binary trees of various sizes,
   where smaller trees will have shorter lifetimes. Balanced so that same memory always allocated
   Also keeps 1 long lived tree
   Really a mix of several different potential benchmarks.
   might want to consider writing JS tester to call each of them in turn *)

(* references used so that tree can be built both top-down and bottom up *)
type tree = Leaf | Node of tree ref * tree ref

let rec build_top_down = function
  | 0 -> Leaf
  | n ->
    let l = ref Leaf and r = ref Leaf in
    let node = Node (l, r) in
    l := build_top_down (n-1);
    r := build_top_down (n-1);
    node

let rec build_bottom_up = function
  | 0 -> Leaf
  | n -> Node (ref (build_bottom_up (n-1)), ref (build_bottom_up (n-1)))

(* Bit shift operators not implemented *)
let treeSize n =
  let rec help acc = function
    | 0 -> acc
    | n -> help (2*acc) (n-1)
  in help 1 n

(* TODO: Select a good default *)
(*
  Size  |  Peak Memory  |  GC calls
  10    |    460KB      |   16
  12    |    1.8MB      |   42
  14    |    6.9MB      |   135
  16    |    27MB       |   506    (about 5 seconds to execute)
*)
let maxSize = 16

(* Ensures approximately the same overall memory allocated for each size *)
let iterations n = (treeSize (maxSize - n))

let result1 = ref Leaf (* store result in a global so it won't be optimised out *)
let result2 = ref Leaf

(* TODO: Can stop before 0, and can go in steps of 2 rather than every size *)
let rec create_trees = function
  | 2 -> ()
  | n ->
    let iters = iterations n in
    let rec generate = function
      | 0 -> ()
      | i ->
     (*   result1 := build_top_down n; *)
        result2 := build_bottom_up n;
        generate (i-1) in
    generate iters; create_trees (n-2)

(* create a long lived tree, then generate and discard a bunch of trees *)
let t =
  let a = build_bottom_up maxSize in
  create_trees maxSize; a
