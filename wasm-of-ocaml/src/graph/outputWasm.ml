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
  | LocalSet i -> Ast.LocalSet i :: rest
  | LocalTee i -> Ast.LocalTee i :: rest
  | GlobalGet i -> Ast.GlobalGet i :: rest
  | GlobalSet i -> Ast.GlobalSet i :: rest
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

(* use prog.num_globals *)
let shadow_stack_locals (types : Ast.type_ list) {ftype; locals; num_swaps; body} =
  if !(Compilerflags.no_gc) then body else
  let arity = match List.nth types (Int32.to_int ftype.it) with
    {it=Wasm.Types.FuncType (args, _)} -> List.length args in
  let num_stack_spaces = arity + (List.length locals) - num_swaps in
  (List.map add_dummy_edges (Compilewasm.call_create_fun num_stack_spaces)) @
    (* Because intermediate closures of curried function calls (function applied many times in one place)
       aren't stored anywhere other than a swap variable, the closure needs to be explicitly put on the stack
       at the start of a function. (Check args > 0 to avoid case of MAIN, which has no args/closures) *)
    (* TODO: Tidy to make more efficient, insert only between curried calls and avoid needing to re-tag *)
    (if arity = 0 then []
      else List.map add_dummy_edges (
      [LocalGet (add_dummy_loc 0l); ] @ (Compilewasm.toggle_tag Bindstree.Closure)
       @ (Compilewasm.update_local 0l) @ [Drop] )) @
    body @ (List.map add_dummy_edges (Compilewasm.call_exit_fun num_stack_spaces))

let translate_funcs types globals funcs =
  let other_funcs, main = Utils.split_last [] funcs in
  (* As well as leaving stack space for the locals of main, it first leaves some space for globals.
     Could merge both of the create_fun calls at start into 1, but overhead is minimal and makes purpose clearer *)
  let new_main = (List.map add_dummy_edges (Compilewasm.call_create_fun (List.length globals)))
    @ (shadow_stack_locals types main) in
  (List.map (fun ({ftype; locals; body} as f) ->
      add_dummy_loc {ftype; locals;
      Ast.body= List.map add_dummy_loc (translate_body (shadow_stack_locals types f))}) other_funcs) @
    [add_dummy_loc {ftype=main.ftype; locals=main.locals; Ast.body = List.map add_dummy_loc (translate_body new_main)}]

let translate_to_wasm {types; globals; tables; funcs; elems; data; imports; exports} =
  add_dummy_loc {
  types;
  globals = translate_globals globals;
  tables; memories=[];
  funcs = translate_funcs types globals funcs;
  Ast.start=None;
  elems; data; imports; exports;
  }
