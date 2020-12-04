(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * Contributed by Troestler Christophe
 * Rough parallelization by Mauricio Fernandez
 * *reset*

 Binary trees #5
 https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/binarytrees-ocaml-5.html
 *)

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
(* if d = 0 then Empty *)
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

let rec check = function Empty -> 0 | Node(l, r) -> 1 + check l + check r


let min_depth = 4
(* Changed to not both with argv input. Default 10, test suggests 21. Will be limited by memory size? *)
(* In original benchmark: 7 -> 0.11s, 14 -> 0.03s, 21 -> 9.4s *)
let max_depth = 10
let stretch_depth = max_depth + 1


let check_count = check (make stretch_depth)
  (*
  let c = check (make stretch_depth) in
  Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c*)

let long_lived_tree = make max_depth


(* Replaced ref with own definition, since ref not yet implemented *)
type 'a ref = {mutable content: 'a}
(* Also replaced Array.init with list function since modules not implemented so only have literal arrays *)

let init n f =
  let rec help i n f =
    if i = n then [] else (f i)::(help (i+1) n f)
  in help 0 n f
let rec iter f = function
  | [] -> ()
  | x::xs -> (f x; iter f xs)


(* TODO: Work out what is actually does *)
let rec loop_depths d =
  for i = 0 to  ((max_depth - d) / 2 + 1) - 1 do
    let d = d + i * 2 in
    (* lsl not implemented!!! *)
    let niter = 1 lsl (max_depth - d + min_depth) in (* --- Causes a Not_found error *)
    ()
  (*  let c = (*{content = 0}*) 5 in () *)
   (*   for i = 1 to niter do c.content <- c.content + check(make d) done *)
    (*  Printf.printf "%i\t trees of depth %i\t check: %i\n" niter d !c; *)
  done

(*
let () = loop_depths min_depth
let long_lived_check = check long_lived_tree
  (*Printf.printf "long lived tree of depth %i\t check: %i\n"
    max_depth (check long_lived_tree)*)
*)

