(*
In Wasm:
f = function 7 -> fun 9 -> 11 -> 13 -> 15 -> result
g = function 8 -> 10 -> 12 -> 14 -> 16 -> result

Odd functions do loads/stores to copy across all arguments (very wasteful).
Even functions just create a [func_idx] each time as only the last arg is used.

In practice, expect every argument to be needed so no added cost in always keeping them.
Added cost is just that initial closure has space for all the variables it will take, rather than 1 at a time,
and the 2 header fields needed to track/copy environments.
*)

let f a b c d e = a + b + c + d + e
let g a b c d e = e
let x = f 1 2 3 4 5
let y = g 1 2 3 4 5
