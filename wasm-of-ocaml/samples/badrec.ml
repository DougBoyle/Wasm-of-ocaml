(* No reason to use 'let rec' unless defining a new recursive function, but the OCaml syntax still allows it *)
let f x = x
let rec g = f
