let rand n = (10 mod n)

let reverse _ = ()

(* Removing the 'let c = rand 3' removes error *)
(* replacing let () with let _ also removes error *)
let () = rand 3;
   (* Removing if statement removes error *)
   (* Removing 'reverse' also removes error??? *)
   (* TODO: If false and if 1 = 2 behave differently, should be the same? *)
   if 1 = 2 then () else reverse (rand 5)
