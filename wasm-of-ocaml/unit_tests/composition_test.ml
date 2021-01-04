(* Regression test for old bug in translation to graph, where 'else' branch of if statement not processed *)
let rand n = (10 mod n)

let reverse _ = ()

let () =
  let c = rand 3 in
  if c = 0 then () else reverse (rand 5)
