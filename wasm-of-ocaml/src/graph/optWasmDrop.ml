(* Dead assigment elimination introduces 'Drop' instructions. In some places, should be able
   to propagate these backwards to remove the thing that produced them. Have to be aware of effectful
   instructions like call/set, and changes in arity like i32.binary *)

