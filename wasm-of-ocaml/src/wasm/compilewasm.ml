open Wasmtree
open Wasm
(* TODO: Use concatList to enable pre/post append efficiently *)
(* Grain uses deque package: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatDeque.html
   and similar lists: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatList.html *)

(* Add parts as they appear needed, don't include whole 1100 line file at once *)

(* Associate a dummy location to a value *)
let add_dummy_loc (x : 'a) : 'a Source.phrase = Source.(x @@ no_region)

type env = {
  (* (* Pointer to top of heap (needed until GC is implemented) *)
  heap_top: Wasm.Ast.var; -- likely GC related *)
  num_args: int;
  func_offset: int;
  global_offset: int;
(*  import_global_offset: int;
  import_func_offset: int;  -- unclear how much knowledge about imports will be needed
  import_offset: int;  *)

 (* func_types: Wasm.Types.func_type BatDeque.t ref;   Don't include need for double-ended queue currently *)
 func_types : Wasm.Types.func_type list ref;


  (* Allocated closures which need backpatching *)
  backpatches: (Wasm.Ast.instr' (* Concatlist.t  *) list * closure_data) list ref;
  imported_funcs: (int32 Ident.tbl) Ident.tbl;  (* TODO: May not be necessary if imports fixed to just the OCaml runtime parts *)
 (* imported_globals: (int32 Ident.tbl) Ident.tbl;  *)
}

(* These are the bare-minimum imports needed for basic runtime support -- Many removed, see grain compcore.ml *)
(* Ignore anything exception or printing related (could technically handle specific print cases with js calls) *)
(* Primitives still not translated to idents at this stage - aids constant propagation if I want to do at this level *)
let runtime_mod = Ident.create_persistent "ocamlRuntime"
let alloc_ident = Ident.create_persistent "alloc"

let runtime_global_imports = []
let runtime_function_imports = [
  {
    mimp_mod=runtime_mod;
    mimp_name=alloc_ident;
    mimp_type=MFuncImport([I32Type], [I32Type]);
    mimp_kind=MImportWasm;
    mimp_setup=MSetupNone;
  };]
let runtime_imports = runtime_global_imports @ runtime_function_imports

(* TODO: Support strings *)

let const_int32 n = add_dummy_loc (Values.I32Value.to_value (Int32.of_int n))
let const_int64 n = add_dummy_loc (Values.I64Value.to_value (Int64.of_int n))
let const_float32 n = add_dummy_loc (Values.F32Value.to_value (Wasm.F32.of_float n))
let const_float64 n = add_dummy_loc (Values.F64Value.to_value (Wasm.F64.of_float n))

(* These are like the above 'const' functions, but take inputs
   of the underlying types instead *)
let wrap_int32 n = add_dummy_loc (Values.I32Value.to_value n)
let wrap_int64 n = add_dummy_loc (Values.I64Value.to_value n)
let wrap_float32 n = add_dummy_loc (Values.F32Value.to_value n)
let wrap_float64 n = add_dummy_loc (Values.F64Value.to_value n)

(** Constant compilation *)
let rec compile_const c : Wasm.Values.value =
  let conv_int32 = Int32.(mul (of_int 2)) in
  let conv_int64 = Int64.(mul (of_int 2)) in
  begin
    match c with
    | MConstLiteral ((MConstLiteral _) as c) -> compile_const c
    | MConstI32 n -> Values.I32Value.to_value (conv_int32 n)
    | MConstI64 n -> Values.I64Value.to_value (conv_int64 n)
    | MConstF32 n -> Values.F32Value.to_value (Wasm.F32.of_float n)
    | MConstF64 n -> Values.F64Value.to_value (Wasm.F64.of_float n)
    | MConstLiteral (MConstI32 n) -> Values.I32Value.to_value n
    | MConstLiteral (MConstI64 n) -> Values.I64Value.to_value n
    | MConstLiteral (MConstF32 n) -> Values.F32Value.to_value (Wasm.F32.of_float n)
    | MConstLiteral (MConstF64 n) -> Values.F64Value.to_value (Wasm.F64.of_float n)
  end

(* Translate constants to WASM. Override names from wasmtree to be wasm phrases (values with regions) *)
let const_true = add_dummy_loc (compile_const const_true)
let const_false = add_dummy_loc (compile_const const_false)

(* WebAssembly helpers *)
(* These instructions get helpers due to their verbosity *)
let store
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0)
    ?sz:(sz=None)
    () =
  let open Wasm.Ast in
  Store({ty; align; sz; offset=Int32.of_int offset;})

let load
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0)
    ?sz:(sz=None)
    () =
  let open Wasm.Ast in
  Load({ty; align; sz; offset=Int32.of_int offset;})

(* Unclear if global vars/lookups needed (rather than just func) *)
let lookup_ext_func env modname itemname =
  Ident.find_same itemname (Ident.find_same modname (env.imported_funcs))
let var_of_ext_func env modname itemname =
  add_dummy_loc @@ lookup_ext_func env modname itemname
let call_alloc env = Ast.Call(var_of_ext_func env runtime_mod alloc_ident)

(* Equivalent to BatDeque.find but on lists *)
let find_index p l =
  let rec aux n = function [] -> None | x::xs -> if p x then Some(n, x) else aux (n+1) xs in
  aux 0 l

(* Rewritten to not bother with a BatDeque *)
let get_func_type_idx env typ =
  match find_index ((=) typ) !(env.func_types) with
  | None ->
    (* TODO: Inefficient, BatDeque does O(1) snoc and size *)
    env.func_types := !(env.func_types) @ [typ];
    List.length !(env.func_types) - 1
  | Some((i, _)) -> i

let get_arity_func_type_idx env arity =
  let has_arity (Types.FuncType(args, _)) = (List.length args) = arity in
  match find_index has_arity !(env.func_types) with
  | None ->
    let args = List.init arity (fun _ -> Types.I32Type) in
    let ftype = (Types.FuncType(args, [Types.I32Type])) in
    env.func_types := !(env.func_types) @ [ftype];
    List.length !(env.func_types) - 1
  | Some((i, _)) -> i

(* Up to line 225 in grain/codegen/compcore.ml *)

(* Meaning of untag? Look at its uses *)