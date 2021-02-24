(* Tests gc behaviour for large number of pages and objects of varying size *)
type list = Nil | Cons1 of int * list | Cons2 of int * int * list

let longLivedList = ref Nil
let shortFreedList = ref Nil
let longFreedList = ref Nil

let rec buildLists = function
  | 0 -> ()
  | n ->
    (if (n mod 20) = 0 then longFreedList := Cons2(n, n, !longFreedList)
    else if n mod 2  = 0 then shortFreedList := Cons1(n, !shortFreedList)
    else longLivedList := Cons1(n, !longLivedList));
    buildLists (n-1)

let _ = buildLists 8000


