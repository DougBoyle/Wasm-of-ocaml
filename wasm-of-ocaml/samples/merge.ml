(* Common example of issues in pattern matching, due to mix of variables/constructor cases *)
let rec merge lx ly = match (lx, ly) with
  | [], _ -> ly
  | _, [] -> lx
  | x::xs, y::ys -> x::y::(merge xs ys)

let a = merge [1] [2]
let b = (a = [1;2])