(* Example given by the optimised pattern matching paper (it uses n = 38) *)
let rec fib = function
  | (0|1) -> 1
  | n -> fib (n-1) + fib (n-2)

let x = fib 38
