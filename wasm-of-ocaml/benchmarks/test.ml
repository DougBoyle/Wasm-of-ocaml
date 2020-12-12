(* Tries to show the profile cost of composing small functions.
https://github.com/Chris00/ocaml-benchmark/blob/master/examples/composition.ml
Actual package has a 'throughput' function which runs code for a specified number of seconds.
Just use a for loop for this.
*)

(* Small functions: permutations of [0 .. n-1] *)


(* Was originally defined on arrays, now defined on lists *)
(* : int -> (int -> int) list * (int list -> int list -> unit) list *)
  (* Create a random list of transformations - No random module, have to just pick a number! *)
let rec random_perm (p_f, p_v) _ =
  random_perm (p_f, p_v) 0

let _ = random_perm ([], []) 0
