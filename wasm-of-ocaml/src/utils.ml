(* Utility functions used in various files - usually based on an extension package list BatList *)

(* Fold left but with an added integer argument *)
let rec fold_lefti f e l =
  fst (List.fold_left (fun (e, i) x -> (f e i x, i+1)) (e, 0) l)

let rec take n l = if n = 0 then ([], l) else match l with
  | [] -> assert false (* Should never happen *)
  | x::xs -> let (h, t) = take (n-1) xs in (x::h, t)
