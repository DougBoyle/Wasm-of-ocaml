let rec split (l1, l2) = function
  | [] -> (l1, l2)
  | x::xs -> split (l2, x::l1) xs

let rec merge l1 l2 = match l1, l2 with
  | [], l -> l
  | l, [] -> l
  | x::xs, y::ys when x < y -> x :: (merge xs (y::ys))
  | l, y::ys -> y::(merge l ys)

let rec mergesort = function
  | [] -> []
  | [x] -> [x]
  | l ->
    let l1, l2 = split ([], []) l in
    merge (mergesort l1) (mergesort l2)

let rec nth (x::xs) = function
  | 0 -> x
  | n -> nth xs (n-1)

let l = [4; 9; 7; 3; 6]
