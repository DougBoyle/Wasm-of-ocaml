(*
  The expression "let _ = init a 10" is unused but is not removed due to not being able to
  determine that the recursive function init is safe.
  Despite this, it gets garbage collected so that final memory usage with gc is 1160, and is 1480 without
*)

let rec init acc = function
  | 0 -> acc
  | n -> init (n::acc) (n-1)

let geta _ =
  let a = init [] 10 in
  let _ = init a 10 in
  init a 10

let x =
  let a = geta () in
  init a 10

