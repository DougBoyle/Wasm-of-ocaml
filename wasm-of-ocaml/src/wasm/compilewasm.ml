open Wasmtree
open Wasm
(* TODO: Use concatList to enable pre/post append efficiently *)
(* Grain uses deque package: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatDeque.html
   and similar lists: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatList.html *)

(* Add parts as they appear needed, don't include whole 1100 line file at once *)

(* Associate a dummy location to a value - Wasm.Ast.instr = instr' Source.phrase so every part needs location *)
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
(* TODO: Encoded int n = n*2 -- Shouldn't be necessary due to no GC
         IS AFFECTED BY FACT OCAML EXPECTS 31-BIT INTEGERS, NOT 32 - COME BACK TO ONCE WORKING, OVERFLOWS ARE RARE
*)

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

(* Untag should only be needed if GC present (especially untag_number) *)
(* encode after doing comparison etc. decode before an if/while statement *)
let encode_bool =  [
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.Shl);
  Ast.Const(const_false);
  Ast.Binary(Values.I32 Ast.IntOp.Or);
]

let decode_bool = [
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.ShrU);
]

(* Can remove - not encoding integers *)
let encoded_const_int32 n = const_int32 n

(* Wasm package changed from GetLocal to LocalGet (likewise for the rest) since Grain's version *)
let compile_bind ~is_get (env : env) (b : binding) : Wasm.Ast.instr' list =
  let (++) a b = Int32.(add (of_int a) b) in
  match b with
  | MArgBind(i) ->
    (* No adjustments are needed for argument bindings *)
    let slot = add_dummy_loc i in
    if is_get then
      [Ast.LocalGet(slot)]
    else
      [Ast.LocalSet(slot)]
  | MLocalBind(i) ->
    (* Local bindings need to be offset to account for arguments and swap variables *)
    let slot = add_dummy_loc ((env.num_args (* + (List.length swap_slots)*)) ++ i) in
    if is_get then
     [Ast.LocalGet(slot)]
    else
     [Ast.LocalSet(slot)]
 (* Swap bindings actually needed or not?
 | MSwapBind(i) ->
    (* Swap bindings need to be offset to account for arguments *)
    let slot = add_dummy_loc (env.num_args ++ i) in
    if is_get then
      singleton (Ast.GetLocal(slot))
    else
      singleton (Ast.SetLocal(slot))    *)
  | MGlobalBind(i) ->
    (* Global bindings need to be offset to account for any imports *)
    let slot = add_dummy_loc (env.global_offset ++ i) in
    if is_get then
      [Ast.GlobalGet(slot)]
    else
      [Ast.GlobalSet(slot)]
  | MClosureBind(i) ->
    (* Closure bindings need to be calculated *)
    begin
      if not(is_get) then
        failwith "Internal error: attempted to emit instruction which would mutate closure contents"
    end;
      (* Closure is always arg 0? *)
      (Ast.LocalGet(add_dummy_loc Int32.zero))::
      [load ~offset:(4 * (3 + Int32.to_int i)) ()]
  | MImport(i) ->
    begin
      if not(is_get) then
        failwith "Internal error: attempted to emit instruction which would mutate an import"
    end;
    (* Adjust for runtime functions *)
    let slot = add_dummy_loc ((* env.import_offset ++  should be fixed *) i) in
    [Ast.GlobalGet(slot)]

(* Get/Set swap left out - unclear if they will actually be needed *)

let compile_imm (env : env) (i : immediate) : Wasm.Ast.instr' list =
  match i with
  | MImmConst c -> [Ast.Const(add_dummy_loc @@ compile_const c)]
  | MImmBinding b -> compile_bind ~is_get:true env b
  | MFail i -> failwith "Need to implement Fail - likely requires passing around something to track where handlers are"

(* call_error_handler left out - not doing proper exceptions (makes a call to runtime_throw_error) *)
(* Don't think error_if_true or check_overflow needed either - OCaml allows slient overflows *)

let compile_unary env op arg : Wasm.Ast.instr' list =
  let compiled_arg = compile_imm env arg in
  match op with
  | UnAdd -> [] (* Does nothing *)
  | UnNeg -> (Ast.Const(encoded_const_int32 0)) :: compiled_arg @ [Ast.Binary(Values.I32 Ast.IntOp.Sub)]
  | Not -> compiled_arg @ [
      Ast.Const(const_int32 0x80000000);
      Ast.Binary(Values.I32 Ast.IntOp.Xor);
    ]
  | Succ -> compiled_arg @ [
      Ast.Const(encoded_const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
    ]
  | Pred -> compiled_arg @ [
      Ast.Const(encoded_const_int32 1);
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]
  | Abs -> failwith "Not yet implemented - come back to later"

let compile_binary (env : env) op arg1 arg2 : Wasm.Ast.instr' list =
  let compiled_arg1 = compile_imm env arg1 in
  let compiled_arg2 = compile_imm env arg2 in
(*
  TODO: What is the purpose of these, why are they necessary?
let swap_get = get_swap ~ty:Types.I32Type env 0 in
  let swap_set = set_swap ~ty:Types.I32Type env 0 in
  *)
  (*
  TODO: Not thinking about overflows/31-bit integers initially, do this once rest of program implemented

  let overflow_safe instrs =
    let compiled_swap_get = get_swap ~ty:Types.I64Type env 0 in
    let compiled_swap_set = set_swap ~ty:Types.I64Type env 0 in
    instrs @
    compiled_swap_set @
    compiled_swap_get @
    compiled_swap_get @
    (check_overflow env) @
    compiled_swap_get +@ [
      Ast.Convert(Values.I32 Ast.IntOp.WrapI64);
    ] in
   *)

  match op with
  | Add ->
    (* TODO: Removed overflow_safe bit - commented out in first case, removed in rest *)
   (* overflow_safe @@   *)
    compiled_arg1 @ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 @ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Add);
    ]
  | Sub ->
    compiled_arg1 @ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 @ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Sub);
    ]
  | Mult ->
    (* Untag one of the numbers:
       ((a * 2) / 2) * (b * 2) = (a * b) * 2
    *)
    compiled_arg1 @
   (* untag_number @  -- May actually be needed due to 31-bit OCaml ints *) [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
    ] @
    compiled_arg2 @ [
      Ast.Convert(Values.I64 Ast.IntOp.ExtendSI32);
      Ast.Binary(Values.I64 Ast.IntOp.Mul);
    ]
  (* TODO: Divide and Modulo *)
  (* Can still occur due to how && and || are compiled when not applied to anything??
     i.e. when they are compiled to function abstractions, still use AND/OR rather than rewriting as an if-then-else *)
  | AND ->
    compiled_arg1 @
  (*  swap_set @            -- If I determine that these are necessary, go back to Grain file to see original AND case
    swap_get @  *)
    decode_bool @ [
      Ast.If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_loc compiled_arg2,
             List.map add_dummy_loc  compiled_arg1)  (* swap_get replaced with compiled_arg1 - may cause some duplication *)
    ]
  | OR ->
    compiled_arg1 @
    decode_bool @ [
      Ast.If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_loc compiled_arg1,
             List.map add_dummy_loc compiled_arg2)
    ]
  (* TODO: Rewrite to call runtime compare function to do more general 'a * 'a comparison *)
  | GT ->
    compiled_arg1 @ compiled_arg2 @ [
      Ast.Compare(Values.I32 Ast.IntOp.GtS)
    ] @ encode_bool
  | GTE ->
    compiled_arg1 @ compiled_arg2 @ [
      Ast.Compare(Values.I32 Ast.IntOp.GeS)
    ] @ encode_bool
  | LT ->
    compiled_arg1 @ compiled_arg2 @ [
      Ast.Compare(Values.I32 Ast.IntOp.LtS)
    ] @ encode_bool
  | LTE ->
    compiled_arg1 @ compiled_arg2 @ [
      Ast.Compare(Values.I32 Ast.IntOp.LeS)
    ] @ encode_bool
  | Eq ->
    compiled_arg1 @ compiled_arg2 @ [
      Ast.Compare(Values.I32 Ast.IntOp.Eq)
    ] @ encode_bool
  (* TODO: Neq -- Is it worth removing this and compiling to Not (Eq ...)? *)
  (* TODO: Physical equality - should actually be relatively simple, just compare literal/pointer. *)
  | Compare -> failwith "Compare not yet implemented"
  (* Append currently being mapped to a linast expression higher up *)
  | Neq | Min | Max | Eq_phys | Neq_phys | Div | Mod | Append -> failwith "Not yet implemented"

(* Line 470 of original code *)