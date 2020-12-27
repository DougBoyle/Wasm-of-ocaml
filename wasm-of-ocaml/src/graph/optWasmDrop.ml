(* Dead assigment elimination introduces 'Drop' instructions. In some places, should be able
   to propagate these backwards to remove the thing that produced them. Have to be aware of effectful
   instructions like call/set, and changes in arity like i32.binary *)
open Graph
open GraphUtils

(* Function to find point in code where previous argument started being constructed *)

(* TODO: Work out way to test this? *)
(* reverse function body is 'drop_instr' 'rest' where 'rest' is 2nd argument *)
let rec propagate_drop drop_instr = function
  | [] -> [drop_instr]
  (* Can't reach drop anyway, so just remove *)
  | ({it=Unreachable|Return} as instr)::rest -> remove_instr drop_instr; instr::rest
  (* Nops should already have been removed/never introduced *)
  (* Just call twice. Check if first call managed to move Drop. If not, avoid repeating *)
  (* TODO: Ideally want to skip the first Drop back to the position in the code its value is generated,
           then don't need to rely on being able to optimise the top one (i.e. Same as for Set) *)
  | ({it=Drop} as instr)::rest -> (match propagate_drop instr rest with
    | {it=Drop}::_ -> drop_instr::instr::rest (* Couldn't optimise the Drop *)
    | rest -> propagate_drop drop_instr rest
  )
  (* Drop the selecting i32, and the two values it is selecting between. (Don't think select is actually used) *)
  | ({it=Select} as instr)::rest ->
    remove_instr instr;
    let drop1 = add_after drop_instr Drop in
    let drop2 = add_after drop1 Drop in
    (* Remember that order actually reversed here *)
    propagate_drop drop2 (drop1 :: drop_instr :: rest)
  (* Avoid trying to optimise blocks. Due to branches, could end up adding in more Drop instructions than started with *)
  (* Avoid optimising branches, as previous result may actually be needed if Drop is skipped *)
  (* Can't optimise 'Call' without knowing that function is immutable (future optimisation) *)
  | ({it=LocalGet _ | GlobalGet _ | MemorySize | Const _} as instr)::rest ->
    remove_instr drop_instr; remove_instr instr; rest
  (* TODO: Local/GlobalSet requires looking further back for what was on stack behind the set instruction's args *)
  | ({it=LocalTee x} as instr)::rest -> remove_instr drop_instr; {instr with it=LocalSet x}::rest
  (* TODO: Fix up graph (remove load). Assumes no out of bounds memory accesses, should be true for compiled Wasm.
           Also assumes conversions always succeed (not used anyway). *)
  | ({it=Load _ | Test _ | Unary _ | Convert _} as instr)::rest ->
    remove_instr instr; propagate_drop drop_instr rest
 (* | ({it=Store _} as instr)::rest ->   Same issue as Set *)
  (* TODO: Fix graph *)
  | ({it=Compare _ | Binary _} as instr)::rest ->
    remove_instr instr;
    let drop = add_after drop_instr Drop in
    propagate_drop drop (drop_instr :: rest)
  | instrs -> drop_instr::instrs (* do nothing *)

let rec remove_drops = function
  | [] -> []
  | ({it=Drop} as instr)::rest -> propagate_drop instr rest
  | instr::rest -> instr::(remove_drops rest)

let optimise ({funcs} as module_) =
  {module_ with funcs=List.map (fun ({body} as f) -> {f with body=List.rev (remove_drops  (List.rev body))}) funcs}
