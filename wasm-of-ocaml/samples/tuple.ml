(* Where function isn't exported and we can guarentee (by a very basic/conservative analysis)
   it is only ever fully/over applied, can replace currying with application as a tuple.
   In theory could do this whenever a function is guarenteed to be applied >= 2 times,
   but not expected to be as useful and means the optimisation has to do more work. *)

let x =
  let adder a b c = a + b + c in
  let rec iterate n a b =
    if n = 0 then adder n a b
    else iterate (n-1) (adder n a b) a in
  iterate 3 2 1

(*
Due to simplicity, program is already massively simplified by inlining.
But for large functions that wouldn't be inlined, especially if they have many arguments, can be very useful.
*)