(* Utility functions used in various files - usually based on an extension package BatList *)

(* Fold left but with an added integer argument *)
let rec fold_lefti f e l =
  fst (List.fold_left (fun (e, i) x -> (f e i x, i+1)) (e, 0) l)

let rec take n l = if n = 0 then ([], l) else match l with
  | [] -> assert false (* Should never happen *)
  | x::xs -> let (h, t) = take (n-1) xs in (x::h, t)

let rec split_last acc = function
  | [] -> failwith "Can't get last element of empty list"
  | [x] -> List.rev acc, x
  | x::xs -> split_last (x::acc) xs