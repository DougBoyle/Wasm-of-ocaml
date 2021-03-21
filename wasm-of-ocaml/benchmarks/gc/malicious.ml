(* Allocate a handful of large objects mixed among enough small objects to use up whole page,
   allow large objects to be release. Then allocate same number again.
   Should see large inefficiency scanning whole of memory to find to free large blocks.
   Size needs to fit size of bins being used!
*)

(* Cons1 cells will allocate 8 word block:
  [header (2 cells) | tag | arity | int | ptr | trailer (2 cells)]
   Cons2 cells will need 10 word block due to extra field.

page = 2^14 = 16384 words
100 cons2 cells = 1000 words
1900 cons1 cells = 15200 words
together = 16200 words so only 184 words = 19 cons2 cells remaining.

2000 allocations, a cons2 cell every 20

Hence allocating another 100 cons2 cells should cause memory to be scanned about 80 times.
If evenly distributed through memory, should see poor performance


Still about 50% slower. Modified to use 2 whole pages instead.

*)

type list = Nil | Cons1 of int * list | Cons2 of int * int * list

let longLivedList = ref Nil
let shortFreedList = ref Nil
let longFreedList = ref Nil

let rec buildLists = function
  | 0 -> ()
  | n ->
    (* even cells get put on a list that gets freed. Every 10th free block is a large block *)
    (if (n mod 20) = 0 then longFreedList := Cons2(n, n, !longFreedList)
    else if (n mod 2) = 0 then shortFreedList := Cons1(n, !shortFreedList)
    else longLivedList := Cons1(n, !longLivedList));
    buildLists (n-1)

(* just allocate the larger cells *)
let rec buildLongList = function
  | 0 -> ()
  | n -> longFreedList := Cons2(n, n, !longFreedList); buildLongList (n-1)

let _ =
  buildLists 8040;
  shortFreedList := Nil; (* Lots of fragmented small blocks to scan over now *)
  (* As number of repetitions increases, new version faster despite more mem overall *)
  (* TODO: Collect data for various limits *)
  for i = 0 to iters do
    longFreedList := Nil; (* some larger blocks among the smaller blocks to allocate in *)
    buildLongList 402
  done


