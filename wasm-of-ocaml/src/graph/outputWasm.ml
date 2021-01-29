open Wasm
open Wasm.Types
open Graph

let translate_globals globals =
  List.map (fun global -> add_dummy_loc {Ast.gtype=global;
    Ast.value=add_dummy_loc [add_dummy_loc (Ast.Const (add_dummy_loc (Values.I32Value.to_value 0l)))]}) globals

let rec translate_instr instr = add_dummy_loc (match instr.it with
  | Unreachable -> Ast.Unreachable
  | Nop -> Ast.Nop
  | Drop -> Ast.Drop
  | Select -> Ast.Select
  | Block (blocktype, body) -> Ast.Block(blocktype, List.map translate_instr body)
  | Loop (blocktype, body) -> Ast.Loop(blocktype, List.map translate_instr body)
  | If (blocktype, body1, body2) -> Ast.If(blocktype, List.map translate_instr body1, List.map translate_instr body2)
  | Br i -> Ast.Br i
  | BrIf i -> Ast.BrIf i
  | BrTable (vars, i) -> Ast.BrTable (vars, i)
  | Return -> Ast.Return
  | Call i -> Ast.Call i
  | CallIndirect i -> Ast.CallIndirect i
  | LocalGet i -> Ast.LocalGet i
  | LocalSet i -> Ast.LocalSet i
  | LocalTee i -> Ast.LocalTee i
  | GlobalGet i -> Ast.GlobalGet i
  | GlobalSet i -> Ast.GlobalSet i
  | Load op -> Ast.Load op
  | Store op -> Ast.Store op
  | MemorySize -> Ast.MemorySize
  | MemoryGrow -> Ast.MemoryGrow
  | Const c -> Ast.Const c
  | Test op -> Ast.Test op
  | Compare op -> Ast.Compare op
  | Unary op -> Ast.Unary op
  | Binary op -> Ast.Binary op
  | Convert op -> Ast.Convert op)

let cleanup_locals (types : Ast.type_ list) {ftype; locals; num_swaps} =
  if !(Compilerflags.no_gc) then [] else
  let arity = match List.nth types (Int32.to_int ftype.it) with
    {it=Wasm.Types.FuncType (args, _)} -> List.length args in
  List.map add_dummy_edges
  (List.flatten
  (* cleanup args. Max needed for MAIN function, which has no args *)
  ((List.init (max 0 (arity - 1))
    (fun i -> (Compilewasm.call_decref
    [LocalGet (add_dummy_loc (Int32.of_int (i + 1)))]) @ [Drop])) @
  (* cleanup locals, but not swap variables *)
  (List.init ((List.length locals) - num_swaps)
    (fun i -> (Compilewasm.call_decref
    [LocalGet (add_dummy_loc (Int32.of_int (i + num_swaps + arity)))]) @ [Drop]))))

let translate_funcs types funcs =
  List.map (fun ({ftype; locals; body} as f) ->
    add_dummy_loc {ftype; locals; Ast.body=List.map translate_instr (body @ (cleanup_locals types f))}) funcs

let translate_to_wasm {types; globals; tables; funcs; elems; data; imports; exports} =
  add_dummy_loc {
  types;
  globals = translate_globals globals;
  tables; memories=[];
  funcs = translate_funcs types funcs;
  Ast.start=None;
  elems; data; imports; exports;
  }
