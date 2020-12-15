(* Simple check that constant propagation occurs *)
(*
 Before:
(let (x/85 = 5
      y/86 = x/85
      compound/87 = y/86
      export z/84 = compound/87) 0)
 After:
(let (x/85 = 5
      compound/87 = x/85      --- y has been removed by CSE. x also removed if const propagation repeated
      export z/84 = compound/87) 0)
 *)
let z =
  let x = 5 in
  let y = x in
  y
