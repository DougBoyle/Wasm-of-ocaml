(*
  Collector runs when allocating would otherwise require adding page(s) to linear memory.
  Therefore, allocate enough garbage that trying to allocate slightly more triggers collector to free up garbage.
  Garbage is circular to demonstrate improvement over reference counting approach.

  Demonstrates intended point when inlining/tailcalls disabled and all Graph optimisations disabled.
  May not have the same effect with those optimisations also taking place (e.g. dead assignments may be eliminated).
*)

(* Can collect circular objects *)
type selfref = {x : int; link : link ref}
and link = Nil | Ref of selfref

(* like above, but end up with a chain of objects all pointing round at each other *)
let build_chain n =
  let rec help chain endref = function
    | 0 -> endref := Ref chain; chain
    | n -> help {x=n; link=ref (Ref chain)} endref (n-1) in
  let endref = ref Nil in
  help {x = n; link = endref} endref (n-1)

let use_memory _ =
  let a = build_chain 30 in
  let _ = build_chain 300 in
  a

let x =
  let a = use_memory () in
  let b = build_chain 200 in
  b.link := Ref a;
  b
