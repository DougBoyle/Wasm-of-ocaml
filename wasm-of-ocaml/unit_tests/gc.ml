(*
  The expression "let _ = init a 10" is unused but is not removed due to not being able to
  determine that the recursive function init is safe (TODO: Optimise this to assume recursive name safe initially).
  Despite this, it gets garbage collected so that final memory usage with gc is 1160, and is 1480 without
*)

(* TODO: Possible optimisation - find the last time a local variable is used and free it then?
     i.e. actual liveness analysis of all locals in wasm.
     Could even do at just the IR level? Would need to work out how to annotate which locals can be used.
     COMPLETELY REMOVE THAT CALCULATION! Rather than being smart, give everything its own local variable
     then do proper REGISTER COLOURING to remove/unify most of them. stack_idx will become a reference? *)

let rec init acc = function
  | 0 -> acc
  | n -> init (n::acc) (n-1)

let geta _ =
  let a = init [] 10 in
  let _ = init a 10 in
  init a 10

let x =
  let a = geta () in
  init a 10

