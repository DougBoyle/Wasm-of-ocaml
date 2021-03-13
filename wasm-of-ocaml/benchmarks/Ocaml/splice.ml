(* Optimised and unoptimised Linast for body of splice function given below:

Unoptimised pattern compilation:
(let (tuple_arg/105 = param/91.(0) tuple_arg/106 = param/91.(1))
 (try
   (let (cstr_tag/107 = tag tuple_arg/105)
     (switch cstr_tag/107
      case tag 0: (let (ly/85 = tuple_arg/106) ly/85)
      default: (fail 0)))
  with (0)
   (try
     (let (cstr_tag/108 = tag tuple_arg/106)
       (switch cstr_tag/108
        case tag 0: (let (lx/86 = tuple_arg/105) lx/86)
        default: (fail 1)))
    with (1)
     (let (cstr_tag/114 = tag tuple_arg/105)
       (switch cstr_tag/114
        case tag 1:
         (let
           (cstr_arg/109 = tuple_arg/105.(0)
            cstr_arg/110 = tuple_arg/105.(1)
            cstr_tag/113 = tag tuple_arg/106)
           (switch cstr_tag/113
            case tag 1:
             (let
               (cstr_arg/111 = tuple_arg/106.(0)
                cstr_arg/112 = tuple_arg/106.(1)
                ys/90 = cstr_arg/112
                y/89 = cstr_arg/111
                xs/88 = cstr_arg/110
                x/87 = cstr_arg/109
                compound/103 = (makeblock 0 xs/88 ys/90)
                fun/104 = splice/84
                compound/102 = (apply fun/104 compound/103)
                compound/101 = (makeblock 1 y/89 compound/102))
               (makeblock 1 x/87 compound/101))
            default: (fail -1)))  -- unnecessary since function is exhaustive
        default: (fail -1))))))  -- unnecessary since function is exhaustive

Optimised pattern compilation:
(let (tuple_arg/105 = param/91.(0) tuple_arg/106 = param/91.(1))
 (try
   (let (cstr_tag/112 = tag tuple_arg/105)
     (switch* cstr_tag/112
      case tag 1:
       (let
         (cstr_field/107 = tuple_arg/105.(0)
          cstr_field/108 = tuple_arg/105.(1)
          cstr_tag/111 = tag tuple_arg/106)
         (switch* cstr_tag/111
          case tag 1:
           (let
             (cstr_field/109 = tuple_arg/106.(0)
              cstr_field/110 = tuple_arg/106.(1)
              ys/90 = cstr_field/110
              y/89 = cstr_field/109
              xs/88 = cstr_field/108
              x/87 = cstr_field/107
              compound/103 = (makeblock 0 xs/88 ys/90)
              fun/104 = splice/84
              compound/102 = (apply fun/104 compound/103)
              compound/101 = (makeblock 1 y/89 compound/102))
             (makeblock 1 x/87 compound/101))
          case tag 0: (fail 0)))
      case tag 0: (let (ly/85 = tuple_arg/106) ly/85)))
  with (0) (let (lx/86 = tuple_arg/105) lx/86)))
*)
let rec splice = function
  | [], ly -> ly
  | lx, [] -> lx
  | x::xs, y::ys -> x::y::(splice (xs, ys))

(* Generate a list of 0s to test with *)
let rec init n =
  if n = 0 then [] else 0::(init (n-1))

let n = 1000
let m = 1000
let a =
  let l = ref [] in
  let l1 = init n and l2 = init n in
  for i = 1 to m do l := splice (l1, l2) done;
  !l

