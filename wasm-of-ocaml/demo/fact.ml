let rec fact = function
  | 0 -> 1
  | n -> n * (fact (n - 1))

let x = fact 5 (* 120 *)
