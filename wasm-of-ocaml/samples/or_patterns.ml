type t = A of int | B of int | C

let y = ref C
let x = match !y with
  | (A x | B x) -> x
  | C -> 0