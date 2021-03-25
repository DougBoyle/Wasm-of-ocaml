(* Example of a fairly complex pattern matching problem *)
let rec merge lx ly = match (lx, ly) with
  | [], _ -> ly
  | _, [] -> lx
  | x::xs, y::ys when x < y -> x::(merge xs ly)
  | _, y::ys -> y::(merge lx ys)

let rec split l1 l2 = function
  | [] -> l1, l2
  | x::xs -> split (x::l2) l1 xs

let rec mergesort = function
  | [] -> []
  | [x] -> [x]
  | l ->
    let l1, l2 = split [] [] l in
    merge (mergesort l1) (mergesort l2)


let a = 214013
let c = 2531011
let m = 65536
let x = ref 12345
let rand n = (x := a*(!x) + c mod m; !x mod n)

let init n m =
  let rec help n =
    if n = 0 then [] else (rand m)::(help (n-1))
  in help n

let n = 3000
let m = 10000
let l = init n m
let sorted_l = mergesort l
