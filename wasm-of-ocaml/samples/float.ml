let x = 1.0
let y = x = 1.0
let z = x = 1.1
(* A bad idea to exactly match floats in programs, but as a test program this ensures implementation correct *)
let w = match x with 1.0 -> 1 | _ -> 0
let v = match x with 1.1 -> 1 | _ -> 0
let a = x +. 1.0
let b = a = 2.0
let c = a = 1.99
let d = sqrt 4.0
let e = d = 2.0
let f = 2.0 = 2.00 (* Any folding of floats must be done on values, not strings *)
