(* 2 iterations of optimisation are needed to remove the dead assignment to b,
   since compiler generates compound/n = bodies.(0); b = compound/n.
   Hence b is removed on first pass, and compound/n on second pass. *)
let advance bodies =
  (* Removing i loop removes error *)
  for i = 0 to 3 do
    (* changing b to _ removes error, changing bodies.(0) to bodies removes error *)
    let b = bodies.(0) in
    (* Removing j for loop removes issue *)
    for j = i to i do
      ()
    done
  done
