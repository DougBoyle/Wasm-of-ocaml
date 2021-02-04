(* Switch GC off *)
let no_gc = ref false

(* Allows switching between basic and optimised pattern matching algorithms *)
let use_optimised_pattern_compilation = ref true

(* if true: variables mapped to distinct locals wherever possible, resolved by clash graph colouring.
   if false: variables mapped/reused according to scoping, still remove unused locals where possible.
   Reducing the number of locals is particularly important with mark-sweep collector,
   as stack frame use memory.*)
let use_colouring = ref true
