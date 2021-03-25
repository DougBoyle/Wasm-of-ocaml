(* Same as constProp.ml but for values assigned into a block and therefore harder to keep track of.
   Value assigned can be a complex object, not just a float/int *)
(*
Before:
(let
  (compound/95 = (makeblock 0) -- shared empty list
   compound/94 = (makeblock 1 1 compound/95)
   export x/84 = compound/94
   compound/89 = (makeblock 1 x/84 compound/95)
   compound/88 =
     (try
       (let (cstr_tag/93 = tag compound/89)
         (switch cstr_tag/93
          case tag 1:
           (let
             (cstr_arg/91 = compound/89.(0) --- Replaced with x and compound/95. Use existing locals rather than
              cstr_arg/92 = compound/89.(1) --- accessing memory again.
              v/86 = cstr_arg/91)
             v/86)
          default: (fail 0)))
      with (0) (makeblock 0))
   export y/85 = compound/88)
  0)

After:
(let
  (compound/95 = (makeblock 0)
   compound/94 = (makeblock 1 1 compound/95)
   export x/84 = compound/94
   compound/89 = (makeblock 1 x/84 compound/95)
   compound/88 =
     (try
       (let (cstr_tag/93 = tag compound/89)
         (switch cstr_tag/93
          case tag 1:
           (let
             (cstr_arg/91 = x/84         -- Uses already allocated local variables.
              cstr_arg/92 = compound/95  -- Guarenteed to be fewer instructions in Wasm.
              v/86 = cstr_arg/91)
             v/86)
          default: (fail 0)))
      with (0) (makeblock 0))
   export y/85 = compound/88)
  0)

*)
let x = [1]

let y = match [x] with | v::vs -> v | _ -> []
