let rec foldl f e l = match l with
  | [] -> e
  | x::xs -> foldl f (f e x) xs

let sum = foldl (+) 0

let rec rev = function
  | [] -> []
  | x::xs -> x::(rev xs)

let init n f =
  let rec help acc i =
    if i = 0 then acc else help ((f i)::acc) (i-1)
  in help [] n

let rec loop f n =
  if n = 0 then f() else let _ = f() in loop f (n-1)

let x =
  let l = init 100 (fun x -> x) in
  loop (fun _ -> sum l) 300
