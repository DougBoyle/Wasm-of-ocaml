(*
Example of mutually recursive functions.
CPS used to mimic an interpreter-like program
*)

(* Hofstadter Female and Male sequences *)
(*
  let rec f n = if n = 0 then 1 else n - (m (f (n-1)))
  and m n = if n = 0 then 0 else n - (f (m (n-1)))
*)

(* CPS version *)
type action = Sub of int | F | M
let rec process n = function
  | [] -> n
  | (Sub m) :: acts -> process (m - n) acts
  | F :: acts -> do_f n acts
  | M :: acts -> do_m n acts
and do_f n acts =
  if n = 0 then process 1 acts
  else process (n-1) (F::M::(Sub n)::acts)
and do_m n acts =
  if n = 0 then process 0 acts
  else process (n-1) (M::F::(Sub n)::acts)

let f n = process n [F]
let m n = process n [M]

let a = f 15
