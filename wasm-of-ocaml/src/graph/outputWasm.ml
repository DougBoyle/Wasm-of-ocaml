open Wasm
open Wasm.Types
open Graph

let call_create_fun num_args =
  if !(Compilerflags.no_gc) then [] else [Const(Compilewasm.const_int32 (num_args * 4));
  Call(Compilewasm.var_of_runtime_func Compilewasm.create_fun_ident)]
let call_exit_fun num_args =
  if !(Compilerflags.no_gc) then [] else [Const(Compilewasm.const_int32 (num_args * 4));
  Call(Compilewasm.var_of_runtime_func Compilewasm.exit_fun_ident)]

let update_local index =
  if !(Compilerflags.no_gc) then []
  else [Ast.Const(Compilewasm.wrap_int32 (Int32.mul (Int32.add index 1l) 4l));
    Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.update_local_ident)]

let update_global index =
  if !(Compilerflags.no_gc) then []
  else [Ast.Const(Compilewasm.wrap_int32 (Int32.mul index 4l)); Ast.GlobalGet(add_dummy_loc index);
  Ast.Store({ty=Wasm.Types.I32Type; align=2; sz=None; offset=0l;})]

let translate_globals globals =
  List.map (fun global -> add_dummy_loc {Ast.gtype=global;
    Ast.value=add_dummy_loc [add_dummy_loc (Ast.Const (add_dummy_loc (Values.I32Value.to_value 0l)))]}) globals

(* Has to be written this way due to Set/Tee possibly mapping to multiple instructions when gc enabled *)
(* arity and num_swaps are so that we know which local variable bindings also need to update the shadow stack *)
let rec translate_body arity num_swaps body =
  List.map add_dummy_loc
  (List.fold_right (fun instr rest -> match instr.it with
  | Unreachable -> Ast.Unreachable :: rest
  | Nop -> Ast.Nop :: rest
  | Drop -> Ast.Drop :: rest
  | Select -> Ast.Select :: rest
  | Block (blocktype, body) -> Ast.Block(blocktype, (translate_body arity num_swaps body)) :: rest
  | Loop (blocktype, body) -> Ast.Loop(blocktype, (translate_body arity num_swaps body)) :: rest
  | If (blocktype, body1, body2) ->
    Ast.If(blocktype, (translate_body arity num_swaps body1),
      (translate_body arity num_swaps body2)) :: rest
  | Br i -> Ast.Br i :: rest
  | BrIf i -> Ast.BrIf i :: rest
  | BrTable (vars, i) -> Ast.BrTable (vars, i) :: rest
  | Return -> Ast.Return :: rest
  | Call i -> Ast.Call i :: rest
  | CallIndirect i -> Ast.CallIndirect i :: rest
  | LocalGet i -> Ast.LocalGet i :: rest
  (* Update shadow stack as required for non-swap local varaible updates *)
  (* TODO: Tidy up - pass in as i32s *)
  | LocalSet i -> let index = i.it in
    (if index < (Int32.of_int arity) then (update_local i.it)
     else if index >= (Int32.(add (of_int arity) (of_int num_swaps)))
       then (update_local (Int32.of_int ((Int32.to_int i.it) - num_swaps)))
     else []
    )
    @ (Ast.LocalSet i :: rest)

  | LocalTee i -> let index = i.it in
    (if index < (Int32.of_int arity) then (update_local i.it)
     else if index >= (Int32.(add (of_int arity) (of_int num_swaps)))
       then (update_local (Int32.of_int ((Int32.to_int i.it) - num_swaps)))
     else []
    )
   @ (Ast.LocalTee i :: rest)

  | GlobalGet i -> Ast.GlobalGet i :: rest

  (* always update shadow stack for global variable updates *)
  | GlobalSet i -> Ast.GlobalSet i :: (update_global i.it) @ rest
  | Load op -> Ast.Load op :: rest
  | Store op -> Ast.Store op :: rest
  | MemorySize -> Ast.MemorySize :: rest
  | MemoryGrow -> Ast.MemoryGrow :: rest
  | Const c -> Ast.Const c :: rest
  | Test op -> Ast.Test op :: rest
  | Compare op -> Ast.Compare op :: rest
  | Unary op -> Ast.Unary op :: rest
  | Binary op -> Ast.Binary op :: rest
  | Convert op -> Ast.Convert op :: rest) body [])

let wrap_function types {ftype; locals; num_swaps} wasm_body =
  if !(Compilerflags.no_gc) then wasm_body else
  let arity = GraphUtils.func_arity types ftype.it in

  (* Number of slots that need allocating on the shadow stack *)
  let num_stack_spaces = arity + (List.length locals) - num_swaps in

  (if num_stack_spaces = 0 then [] else
  (List.map add_dummy_loc [Ast.Const(Compilewasm.const_int32 (num_stack_spaces * 4));
   Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.create_fun_ident)])) @
    (* Because intermediate closures of curried function calls (function applied many times in one place)
       aren't stored anywhere other than a swap variable, the closure needs to be put on the stack
       at the start of a function. (Check args > 0 to avoid case of MAIN, which has no args/closures) *)
    (if arity = 0 then []
      else List.map add_dummy_loc ([Ast.LocalGet (add_dummy_loc 0l);
        Ast.Const(Compilewasm.const_int32 (Bindstree.tag_of_type Bindstree.Closure));
        Ast.Binary(Values.I32 Ast.IntOp.Xor);]
       @ (update_local 0l) @ [Ast.Drop] )) @
    wasm_body @
    (if num_stack_spaces = 0 then [] else
      (List.map add_dummy_loc [Ast.Const(Compilewasm.const_int32 (num_stack_spaces * 4));
        Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.exit_fun_ident)]))

let translate_funcs types globals funcs =
  let other_funcs, main = Utils.split_last [] funcs in

  let wasm_main = (translate_body 0 main.num_swaps main.body) in

  (* As well as leaving stack space for the locals of main, it first leaves some space for globals.
     Could merge both of the create_fun calls at start into 1, but overhead is minimal and makes purpose clearer *)
  let new_main =
    if !(Compilerflags.no_gc) then wasm_main
    else
      List.map add_dummy_loc ([Ast.Const(Compilewasm.const_int32 ((List.length globals) * 4));
         Ast.Call(Compilewasm.var_of_runtime_func Compilewasm.create_fun_ident)])
      @ (wrap_function types main wasm_main) in


  (List.map (fun ({ftype; locals; body; num_swaps} as f) ->
      add_dummy_loc {ftype; locals;
      Ast.body= wrap_function types f (
      (translate_body (GraphUtils.func_arity types ftype.it) num_swaps body))}) other_funcs) @
    [add_dummy_loc {ftype=main.ftype; locals=main.locals;
    Ast.body = new_main}]

let translate_to_wasm {types; globals; tables; funcs; elems; data; imports; exports} =
  add_dummy_loc {
  types;
  globals = translate_globals globals;
  tables; memories=[];
  funcs = translate_funcs types globals funcs;
  Ast.start=None;
  elems; data; imports; exports;
  }
