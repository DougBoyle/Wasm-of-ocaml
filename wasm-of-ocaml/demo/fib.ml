let fib n =
  let rec help a b = function
    | 0 -> a
    | n -> help b (a+b) (n-1)
  in help 0 1 n

let x = fib 10 (* 55 *)
