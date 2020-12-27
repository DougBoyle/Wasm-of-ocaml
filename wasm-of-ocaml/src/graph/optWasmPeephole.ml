(* Simple peephole optimisations not done elsewhere e.g. set i; get i -> tee i.
   tee i can possibly be removed as a dead assignment, whereas set i; get i can't.
   For now, don't optimise inside of blocks due to complications with branches *)
open Graph
open GraphUtils

let rec getset = function
  | [] -> []
  | ({it=LocalSet ({it=i} as var)} as first)::({it=LocalGet {it=j}} as second)::rest when i = j ->
    merge_instrs first second;
   {first with it = LocalTee var; succ=second.succ}::(getset rest)
  | ({it=Block(typ, body)} as instr)::rest ->
    {instr with it=Block(typ, getset body)}::(getset rest)
  | ({it=Loop(typ, body)} as instr)::rest ->
    {instr with it=Loop(typ, getset body)}::(getset rest)
  | ({it=If(typ, body1, body2)} as instr)::rest ->
    {instr with it=If(typ, getset body1, getset body2)}::(getset rest)
  (* TODO: Also remove Nops while here. Look at deadlocals for function to remove an instruction.
           Put all the graph manipulation functions in one utilities file *)
  | instr::rest -> instr::(getset rest)


let optimise ({funcs} as module_) =
  {module_ with funcs=List.map (fun ({body} as f) -> {f with body=getset body}) funcs}
