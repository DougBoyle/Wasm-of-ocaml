(*
  For basic mark and sweep collector, only collects when memory otherwise needs to grow.
  Therefore, test needs to allocate enough memory to use up a full page (16k 32-bit values)
*)

(*
let rec init acc = function
  | 0 -> acc
  | n -> init (n::acc) (n-1)

(* Allocates just enough that the allocation after this exceeds would exceed the memory already available.
   Available heap size is 2^16 B = 65536.
   2000 32-byte objects (can make more efficient in future) is 32 * 2000 = 64000
   2100 objects is 32*2100 = 67200 so force GC to run *)
let geta _ =
  let a = init [] 100 in
  (* Will use fresh memory up until a whole page is allocated *)
  let _ = init a 900 in
  let _ = init a 900 in
  init a 100

let x =
  let a = geta () in
  (* If geta consumed just under a whole page, this triggers a garbage collection
     which frees up the now dead expressions and uses them to allocate the result for x *)
  init a 100
*)



(* Can collect circular objects! *)
(* Simple way to create circular object by closing the loop after object created *)
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

(* Put into separate function to see if that changes anything *)
(*
let show_gc _ =
  let a = use_memory () in
    let b = build_chain 100 in
    b.link := Ref a;
    b
*)

let x =
  let a = use_memory () in
  let b = build_chain 200 in
  b.link := Ref a;
  b
(*  show_gc () *)

