(* Tests a simple case for removing dead assignments *)
(*
Before:
(let
  (x/85 = 1
   y/86 = 2
   z/87 = 3
   compound/88 = (makeblock 0 x/85 z/87)
   export a/84 = compound/88)
  0)

After:
(let
  (x/85 = 1
   z/87 = 3
   compound/88 = (makeblock 0 x/85 z/87)
   export a/84 = compound/88)
  0)
*)
let a =
  let x = 1 in
  let y = 2 in
  let z = 3 in
  (x, z)
