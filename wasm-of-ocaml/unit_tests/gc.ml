(*
  Tests that the garbage collector is working
  Allocates a large amount of memory, allows some of it to be collected, allocates some more.
  Allows all but a small about to be collected at end.

  1 page in WebAssembly is 2^14 bytes = 4096 32-bit words.
  Each cons cell is 4 words, so a 1024 length list takes up about a page
  [32-bit variant tag, 32-bit arity, hd, tail]
  With garbage collection, actually consumes significantly more as it gets wrapped with an
  8 byte reference counting header and 8 byte free list header.

  Hence each element is actually 32 bytes, only need 512 to take up a page
*)

(* TODO: Possible optimisation - find the last time a local variable is used and free it then?
     i.e. actual liveness analysis of all locals in wasm.
     Could even do at just the IR level? Would need to work out how to annotate which locals can be used.
     COMPLETELY REMOVE THAT CALCULATION! Rather than being smart, give everything its own local variable
     then do proper REGISTER COLOURING to remove/unify most of them. stack_idx will become a reference? *)

let rec init acc = function
  | 0 -> acc
  | n -> init (n::acc) (n-1)

(* Once the function exists, b is no longer needed and can be garbage collected before
   the final result is calculated, keeping the total memory allocation within 1 page. *)

(* Function calls needed so that things go out of scope at different times,
   currently a chain of let bindings all stay in scope until the last one finishes.
   TODO: An issue of doing things by linearising? Rewrite how local variables selected? *)
let geta _ =
  let a = init [] 1000 in
  let _ = init a 1000 in (* Have checked - because init is recursive, very little gets optimised here *)
  a

let x =
  let a = geta () in
  init a 1000

