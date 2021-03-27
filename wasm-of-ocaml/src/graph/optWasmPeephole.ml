(* Simple peephole optimisations not done elsewhere e.g. set i; get i -> tee i.
   tee i can possibly be removed as a dead assignment, whereas set i; get i can't. *)
open Graph
open GraphUtils

let rec peephole = function
  | [] -> []
  | ({it=Block(typ, body)} as instr)::rest ->
    {instr with it=Block(typ, peephole body)}::(peephole rest)
  | ({it=Loop(typ, body)} as instr)::rest ->
    {instr with it=Loop(typ, peephole body)}::(peephole rest)
  | ({it=If(typ, body1, body2)} as instr)::rest ->
    {instr with it=If(typ, peephole body1, peephole body2)}::(peephole rest)

  (* Replace set i; get i -> tee i
     If local i isn't used again, tee i will be remove but set i; get i would not be (appears to be live) *)
  | ({it=LocalSet ({it=i} as var)} as first)::({it=LocalGet {it=j}} as second)::rest when i = j ->
    merge_instrs first second;
    first.succ := !(second.succ); (* Need to preserve using the same reference *)
   {first with it = LocalTee var}::(peephole rest)

  (* Remove get i; set i -- achieves nothing *)
  | ({it=LocalGet {it=i}} as first)::({it=LocalSet ({it=j})} as second)::rest when i = j ->
    remove_instr first;
    remove_instr second;
    peephole rest

  (* Remove Nops *)
  | ({it=Nop} as instr)::rest ->
    remove_instr instr;
    peephole rest
  (* Later instructions never execute, such cases sometimes introduced by switch statements that fail in some case *)
  | ({it=Br _ | Unreachable | BrTable _} as first)::rest ->
    List.iter remove_instr rest;
    [first]

  (* Tags are added/removed by Xor. Rather than requiring some form of constant propagation on the stack,
     just look for the specific pattern of something being immediately tagged/untagged *)
  | ({it=Const i} as first_tag)::({it=Binary(Wasm.Values.I32 Wasm.Ast.IntOp.Xor)} as first_xor)::
    ({it=Const j} as second_tag)::({it=Binary(Wasm.Values.I32 Wasm.Ast.IntOp.Xor)} as second_xor)::rest
    when i = j ->
    List.iter remove_instr [first_tag; first_xor; second_tag; second_xor];
    peephole rest

  | instr::rest -> instr::(peephole rest)


let optimise ({funcs} as module_) =
  {module_ with funcs=List.map (fun ({body} as f) -> {f with body=peephole body}) funcs}
