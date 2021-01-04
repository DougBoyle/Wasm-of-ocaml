(* Example of a fairly complex pattern matching problem *)
let rec merge lx ly = match (lx, ly) with
  | [], _ -> ly
  | _, [] -> lx
  | x::xs, y::ys when x < y -> x::(merge xs ly)
  | _, y::ys -> y::(merge lx ys)

let a = 214013
let c = 2531011
let m = 65536
let x = ref 12345
let rand n = (x := a*(!x) + c mod m; !x mod n)

let init n m =
  let rec help i n =
    if i = n then [] else (rand m)::(help (i+1) n)
  in help 0 n

let n = 4000 (* Appear to be limited by memory constraints of list in C Wasm *)
let m = 100
let l1 = init n m
let l2 = init n m
let l = merge l1 l2
