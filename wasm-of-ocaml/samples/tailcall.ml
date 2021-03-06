(* function can be tail call optimised *)
(*
Effect in IR:
Before:
(export fact_acc/84
     (function acc/85 n/86
       (let (compound/91 = (n/86 = 0))
         (if compound/91 acc/85
           (let (compound/88 = (n/86 * acc/85) compound/89 = (n/86 - 1))
             (apply fact_acc/84 compound/88 compound/89))))))

After:
(export fact_acc/84
     (function acc/94 n/95
       (let
         (mut continue/96 = 1
          mut result_mut/97 = 0
          mut acc/85 = acc/94
          mut n/86 = n/95)
         (seq
           (while continue/96
             (seq (continue/96 <- 0)
               (let
                 (compound/91 = (n/86 = 0)
                  result/98 =
                    (if compound/91 acc/85
                      (let
                        (compound/88 = (n/86 * acc/85)
                         compound/89 = (n/86 - 1))
                        (seq (continue/96 <- 1) (acc/85 <- compound/88)
                          (n/86 <- compound/89) 0))))
                 (seq (result_mut/97 <- result/98) 0))))
           result_mut/97))))

Code is significantly longer, but there is no 'apply' within the function so it uses constant stack size.
For a curried function (see output of alias_test.ml), also avoids accessing memory and will effectively
convert the function to taking a tuple rather than curried arguments, so no extra closures.
*)

let rec fact_acc acc n =
  if n = 0 then acc else fact_acc (n * acc) (n-1)

let a = fact_acc 1 5
