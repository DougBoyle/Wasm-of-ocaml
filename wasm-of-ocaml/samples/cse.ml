(* Basic check that CSE removes repeated bindings, but doesn't try to remove if out of scope *)
(* CSE + Dead Assignment eliminate (without constant propagation)
Before:
(let
  (compound/90 = (if 1 (let (a/85 = 5 b/86 = 5) (a/85 + b/86)) 0)
   export x/84 = compound/90
   c/88 = 5
   compound/89 = c/88
   export y/87 = compound/89)
  0)

After:
(let
  (compound/90 = (if 1 (let (a/85 = 5) (a/85 + a/85)) 0)
   export x/84 = compound/90
   c/88 = 5
   compound/89 = c/88
   export y/87 = compound/89)
  0)
*)
let x =
  if true then
    let a = 5 in
    let b = 5 in
    a + b
  else 0

let y =
  let c = 5 in
  c
