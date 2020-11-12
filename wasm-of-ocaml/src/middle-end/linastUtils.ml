
let defaultLoc = Location.none
let defaultEnv = Env.empty

(* Don't worry about passing around locations/environments for now, just a hastle *)
module Imm = struct
  let mk d =
      {desc=d;
       loc=defaultLoc;
       env=defaultEnv;
       anotations=ref []}
  let id id = mk (ImmIdent id)
  let const const = mk (ImmConst const)
end