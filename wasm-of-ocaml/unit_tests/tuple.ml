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
With neither optimisations: 6 functions, 827 bytes
With just tupling: 2 functions, 546 bytes
With just inlining: 3 functions, 596 bytes (adder inlined and eliminated, iterate not)
With both: 1 functions, 474 bytes
*)