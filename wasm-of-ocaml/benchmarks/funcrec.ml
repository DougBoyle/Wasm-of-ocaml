(* https://github.com/Chris00/ocaml-benchmark/blob/master/examples/func_record.ml
   Originally also had functor over a struct, and was based on floats rather than ints.
   Compares the performance of a simple function done different ways *)

type env = { f : int -> int;  g : int -> int }

let h_record e x = 1 + e.f x + e.g x

(* With arguments *)
let h_function f g x = 1 + f x + g x

let f x = x + 1
let g x = 2 * x

(* Without arguments *)
let h x = 1 + f x + g x

let record = {f = f; g = g} (* Avoid record being reallocated over and over *)

let rec iter f x = function
    | 0 -> x
    | n -> iter f ((f x) mod 1024) (n-1)

let _ = iter (h_record record) 1 10000
let _ = iter (h_function f g) 1 10000
let _ = iter h 1 10000

