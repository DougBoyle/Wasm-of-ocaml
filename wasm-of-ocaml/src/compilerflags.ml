(* Switch GC off *)
let no_gc = ref false

(* Allows switching between basic and optimised pattern matching algorithms *)
let use_optimised_pattern_compilation = ref true

(* Removed option to use scoping rather than register colouring, else more work necessary
   to ensure variables assigned in OR patterns get bound to the same location.
   As a result, calculating stack size needed, and stack_idx in compilebinds, also removed. *)
