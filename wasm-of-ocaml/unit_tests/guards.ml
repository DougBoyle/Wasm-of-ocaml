let x = match (1, 2) with
  | (a, _) when a > 1 -> 0
  | _ -> 1

(* Check that other patterns are still considered when a guard is present *)
let y = match 0 with
  | _ when false -> 0
  | _ -> 1
