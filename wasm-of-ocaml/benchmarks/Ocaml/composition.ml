(* Tries to show the profile cost of composing small functions.
https://github.com/Chris00/ocaml-benchmark/blob/master/examples/composition.ml
Actual package has a 'throughput' function which runs code for a specified number of seconds.
Just use a for loop for this.
*)

(* Small functions: permutations of [0 .. n-1] *)

(* Minimal "random" number gen *)
let a = 214013
let c = 2531011
let m = 65536
let x = ref 12345
let rand n = (x := a*(!x) + c mod m; !x mod n)

(* Size of list (should be array) *)
let n = 100
let rotate r = fun k -> (k + r) mod n
let reverse i j = fun k -> if i <= k && k <= j then j + i - k else k
let splice l i j = fun k ->
  if k < j then if k < i then k else k + l + 1
  else if k <= j + l then k - j + i
  else let k' = k - l - 1 in if k' < i then k' else k

(* Non-implemented functions. Only have literal constructed arrays so must use lists for arbitrary size *)
let init n f =
  let rec help i n =
    if i = n then [] else (f i)::(help (i+1) n)
  in help 0 n
let rec fold_left f e = function
  | [] -> e
  | x::xs -> fold_left f (f e x) xs
let rec map f = function
  | [] -> []
  | x::xs -> (f x)::(map f xs)

(* Was originally defined on arrays, now defined on lists *)
let make_perms (* : int -> (int -> int) list * (int list -> int list -> unit) list *) =
  (* Create a random list of transformations *)
  let rec random_perm ((p_f, p_v) as acc) i =
    if i <= 0 then acc else
      let c = rand 3 in
      (* New function *)
      let p = if c = 0 then rotate (rand n)
        else if c = 1 then reverse (rand n) (rand n)
        else (* c = 2 *) splice (rand n) (rand n) (rand n) in
      (* Corresponding array transformer *)
      let p_vec v = map p v in
      random_perm (p :: p_f, p_vec :: p_v) (i - 1) in
  random_perm ([], [])

let () =
  let ncomp = 100 in
  let p_f, p_v = make_perms ncomp in
  (* Goes through performing a large collection of list manipulations.
     Huge memory allocation due to using lists, array version not currently possible. *)
  let v = init n (fun k -> k) in
  let do_f () =
    let f = fold_left (fun f f0 -> (fun k -> f0(f k))) (fun k -> k) p_f in
    map f v
  and do_v () =
    fold_left (fun v f -> f v) v p_v
  in
  let _ = do_f () in
  let _ = do_v () in ()
