(* https://ocaml.org/learn/tutorials/99problems.html problems 32-34 *)
let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
	
let coprime a b = gcd a b = 1

let phi n =
    let rec count_coprime acc d =
      if d < n then
        count_coprime (if coprime n d then acc + 1 else acc) (d + 1)
      else acc
    in
    if n = 1 then 1 else count_coprime 0 1

let x = gcd 3094 3874 (* 26 *)
let y = phi 10 (* 4 *)
