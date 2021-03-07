(* Tests that CApp case in analysePurity correctly handles over-application of impure functions *)

let z = ref 5

let f x =
  let g x = 5 in
  incr z;
  g

let y =
  (* due to side effect within f but not g, can't dead code eliminate *)
  let a = f 1 2 in
  10

let h x =
  let g x = 5 in
  g

let w =
  (* pure, safe to dead code eliminate *)
  let b = h 1 2 in
  10