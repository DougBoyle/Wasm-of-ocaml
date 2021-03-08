(* Compilation to WebAssembly should only use branch table if cases small + densely packed *)

(* Disable optimisations for this to not be simplified *)
let x =
  match 10 with
    | 1 -> 1
    | 4 -> 4
    | 10 -> 10
    | 17 -> 17
    | 62 -> 62
    | _ -> 8
