open Wasm
open Wasm.Types
open Graph

let translate_globals globals =
  List.map (fun global -> add_dummy_loc {Ast.gtype=global;
    Ast.value=add_dummy_loc [add_dummy_loc (Ast.Const (add_dummy_loc (Values.I32Value.to_value 0l)))]}) globals

(* Has to be written this way due to Set/Tee possibly mapping to multiple instructions when gc enabled *)
let rec translate_body body = 
  List.fold_right (fun instr rest -> match instr.it with
  | Unreachable -> Ast.Unreachable :: rest
  | Nop -> Ast.Nop :: rest
  | Drop -> Ast.Drop :: rest
  | Select -> Ast.Select :: rest
  | Block (blocktype, body) -> Ast.Block(blocktype, List.map add_dummy_loc (translate_body body)) :: rest
  | Loop (blocktype, body) -> Ast.Loop(blocktype, List.map add_dummy_loc (translate_body body)) :: rest
  | If (blocktype, body1, body2) ->
    Ast.If(blocktype, List.map add_dummy_loc (translate_body body1),
      List.map add_dummy_loc (translate_body body2)) :: rest
  | Br i -> Ast.Br i :: rest
  | BrIf i -> Ast.BrIf i :: rest
  | BrTable (vars, i) -> Ast.BrTable (vars, i) :: rest
  | Return -> Ast.Return :: rest
  | Call i -> Ast.Call i :: rest
  | CallIndirect i -> Ast.CallIndirect i :: rest
  | LocalGet i -> Ast.LocalGet i :: rest
  (* Include an incref/decref as required. Done in a block just so it stays as 1 instruction *)
  | LocalSet (i, incr, decr) ->
    if (incr || decr) && (not (!(Compilerflags.no_gc))) then
    ((if incr then [Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.incref_ident)] else []) @
       (if decr then [Ast.LocalGet i; Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.decref_ident); Ast.Drop] else []) @
       [Ast.LocalSet i]) @ rest
    else Ast.LocalSet i :: rest
  | LocalTee (i, incr, decr) ->
    if (incr || decr) && (not (!(Compilerflags.no_gc))) then
    ((if incr then [Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.incref_ident)] else []) @
       (if decr then [Ast.LocalGet i; Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.decref_ident); Ast.Drop] else []) @
       [Ast.LocalTee i]
      ) @ rest
    else Ast.LocalTee i :: rest
  | GlobalGet i -> Ast.GlobalGet i :: rest
  | GlobalSet (i, incr, decr) ->
    if (incr || decr) && (not (!(Compilerflags.no_gc))) then
    ((if incr then [Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.incref_ident)] else []) @
       (if decr then [Ast.GlobalGet i; Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.decref_ident); Ast.Drop] else []) @
       [Ast.GlobalSet i]
      ) @ rest
    else Ast.GlobalSet i :: rest
  | Load op -> Ast.Load op :: rest
  | Store op -> Ast.Store op :: rest
  | MemorySize -> Ast.MemorySize :: rest
  | MemoryGrow -> Ast.MemoryGrow :: rest
  | Const c -> Ast.Const c :: rest
  | Test op -> Ast.Test op :: rest
  | Compare op -> Ast.Compare op :: rest
  | Unary op -> Ast.Unary op :: rest
  | Binary op -> Ast.Binary op :: rest
  | Convert op -> Ast.Convert op :: rest) body []

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
    add_dummy_loc {ftype; locals;
    Ast.body= List.map add_dummy_loc (translate_body (body @ (cleanup_locals types f)))}) funcs

(*    List.map translate_instr (body @ (cleanup_locals types f))}) funcs *)

let translate_to_wasm {types; globals; tables; funcs; elems; data; imports; exports} =
  add_dummy_loc {
  types;
  globals = translate_globals globals;
  tables; memories=[];
  funcs = translate_funcs types funcs;
  Ast.start=None;
  elems; data; imports; exports;
  }
