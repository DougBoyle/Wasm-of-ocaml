(*
  Initial reference counting implementation would call decref on an object with count 0 for this program
*)

let rec f x =
  (* Changing g and h to not be recursive also fixes error - but only with optimisations enabled! *)
(*  let g x = f (x - 1)
  in let h x = g x in *)
  let g x = f (x-1) in
  let h x = (* a + b + c - d +*) g x in
  if x = 0 then x else
   (* Changing h to g removes the error *)
  h x

let b = f 4


