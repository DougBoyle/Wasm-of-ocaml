open Wasmtree
open Wasm
(* TODO: Use concatList to enable pre/post append efficiently *)
(* Grain uses deque package: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatDeque.html
   and similar lists: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatList.html *)

(* Associate a dummy location to a value - Wasm.Ast.instr = instr' Source.phrase so every part needs location *)
let add_dummy_loc (x : 'a) : 'a Source.phrase = Source.(x @@ no_region)

(* Needed as Wasm Ast represents module names (e.g. in import list) as "name" type, which is actually int list *)
let encode_string : string -> int list = Utf8.decode

(* Not implementing module language so number of imports is entirely specified by the runtime implementation.
   Also only contains functions so doesn't affect the indexing of globals. *)
(* Since only imports are the runtime functions (no variables from other modules), all imports are functions
   so also don't need to put them into global variables, can just track the function index offset and nothing else. *)
type env = {
  num_args: int;
  func_offset: int;
  global_offset: int;
  (*  OCaml runtime should only contain functions, so don't need to track globals *)
  (* No modules so only imports are from runtime functions, don't need to remember offset of imports *)
 func_types : Wasm.Types.func_type list ref;
  (* Allocated closures which need backpatching *)
  backpatches: (Wasm.Ast.instr' (* Concatlist.t  *) list * closure_data) list ref;
  (* Number of blocks to jump through to reach each handler in scope.
     Possibly better choices to use than lists but never mind. *)
  handler_heights: (int32 * int32) list;
}

let enter_block ?(n=1) ({handler_heights;} as env) =
  {env with handler_heights = List.map (fun (handler, height) -> (handler, Int32.add (Int32.of_int n) height)) handler_heights}

(* Number of swap variables to allocate *)
let swap_slots_i32 = [Types.I32Type]
let swap_slots_i64 = [Types.I64Type]
let swap_i32_offset = 0
let swap_i64_offset = List.length swap_slots_i32
let swap_slots = List.append swap_slots_i32 swap_slots_i64

(* These are the bare-minimum imports needed for basic runtime support -- Many removed, see grain compcore.ml *)
(* Ignore anything exception or printing related (could technically handle specific print cases with js calls) *)
(* Primitives still not translated to idents at this stage - aids constant propagation if I want to do at this level *)
let runtime_mod = Ident.create_persistent "ocamlRuntime"
let alloc_ident = Ident.create_persistent "alloc"
let compare_ident = Ident.create_persistent "compare"
let abs_ident = Ident.create_persistent "abs"
let min_ident = Ident.create_persistent "min"
let max_ident = Ident.create_persistent "max"
let append_ident = Ident.create_persistent "@"
let make_float_ident = Ident.create_persistent "make_float"

(* Runtime should only import functions, no globals, so only need to track offset due to functions *)
let runtime_imports = [
  { mimp_name=alloc_ident; mimp_type=MFuncImport([I32Type], [I32Type]); };
  { mimp_name=compare_ident; mimp_type=MFuncImport([I32Type; I32Type], [I32Type]); };
  { mimp_name=abs_ident; mimp_type=MFuncImport([I32Type], [I32Type]); };
  { mimp_name=min_ident; mimp_type=MFuncImport([I32Type; I32Type], [I32Type]); };
  { mimp_name=max_ident; mimp_type=MFuncImport([I32Type; I32Type], [I32Type]); };
  { mimp_name=append_ident; mimp_type=MFuncImport([I32Type; I32Type], [I32Type]); };
  { mimp_name=make_float_ident; mimp_type=MFuncImport([F64Type], [I32Type]); };]

let imported_funcs : (Ident.t, int32) Hashtbl.t = Hashtbl.create (List.length runtime_imports)

let init_env = {
  num_args=0;
  func_offset=0;
  global_offset=0;
  func_types=ref [];
  backpatches=ref [];
  handler_heights = [];
}

(* Finds the function number each runtime import was bound to during setup *)
let lookup_runtime_func env itemname =
  Hashtbl.find imported_funcs itemname
let var_of_runtime_func env itemname =
  add_dummy_loc @@ lookup_runtime_func env itemname
let call_alloc env = Ast.Call(var_of_runtime_func env alloc_ident)
let call_compare env = Ast.Call(var_of_runtime_func env compare_ident)
let call_abs env = Ast.Call(var_of_runtime_func env abs_ident)
let call_min env = Ast.Call(var_of_runtime_func env min_ident)
let call_max env = Ast.Call(var_of_runtime_func env max_ident)
let call_append env = Ast.Call(var_of_runtime_func env append_ident)
let make_float env = Ast.Call(var_of_runtime_func env make_float_ident)

(* TODO: Support strings *)

let const_int32 n = add_dummy_loc (Values.I32Value.to_value (Int32.of_int n))
let const_int64 n = add_dummy_loc (Values.I64Value.to_value (Int64.of_int n))
let const_float64 n = add_dummy_loc (Values.F64Value.to_value (Wasm.F64.of_float n))

(* These are like the above 'const' functions, but take inputs
   of the underlying types instead *)
let wrap_int32 n = add_dummy_loc (Values.I32Value.to_value n)
let wrap_int64 n = add_dummy_loc (Values.I64Value.to_value n)
let wrap_float64 env n = [Ast.Const (add_dummy_loc (Values.F64Value.to_value n)); make_float env]

(* TODO: Work out which of these actually needed *)
(* For integers taken out of wasmtree - all tags/ints need doubling (but not memory offsets) *)
let encoded_int n = n * 2
let encoded_int32 n = Int32.mul 2l n
let encoded_const_int n = const_int32 (encoded_int n)
let encoded_const_int32 n = wrap_int32 (encoded_int32 n)
(** Constant compilation *)
let rec compile_const env c =
    match c with
    | MConstI32 n -> [Ast.Const(encoded_const_int32 n)]
    | MConstI64 n -> [Ast.Const(wrap_int64 (Int64.mul 2L n))] (* TODO: Handle I64's and literal I32s *)
    | MConstF64 n -> wrap_float64 env (Wasm.F64.of_float n)

(* Translate constants to WASM. Override names from wasmtree to be wasm phrases (values with regions) *)
let const_true = compile_const init_env const_true
let const_false = compile_const init_env const_false (* also equals unit i.e. () *)

(* WebAssembly helpers *)
(* These instructions get helpers due to their verbosity *)
let store
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0l)
    ?sz:(sz=None)
    () =
  let open Wasm.Ast in
  Store({ty; align; sz; offset;})

let load
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0l)
    ?sz:(sz=None)
    () =
  let open Wasm.Ast in
  Load({ty; align; sz; offset;})

(* Offset of 4, floats have a tag of 01 *)
let load_float = load ~ty:Wasm.Types.F64Type ~offset:3l ()

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

(* Assumes all functions take an int32 - may cause issues when using floats? *)
let get_arity_func_type_idx env arity =
  let has_arity (Types.FuncType(args, _)) = (List.length args) = arity in
  match find_index has_arity !(env.func_types) with
  | None ->
    let args = List.init arity (fun _ -> Types.I32Type) in
    let ftype = (Types.FuncType(args, [Types.I32Type])) in
    env.func_types := !(env.func_types) @ [ftype];
    List.length !(env.func_types) - 1
  | Some((i, _)) -> i

(* Needed so that OCaml equals function can tell if something is an int/data on heap/closure.
   Bools are just ints so tag_num and untag_num also work for bools *)
let encode_num = [Ast.Const(const_int32 1); Ast.Binary(Values.I32 Ast.IntOp.Shl)]
let decode_num = [Ast.Const(const_int32 1); Ast.Binary(Values.I32 Ast.IntOp.ShrS)]

let untag tag = [
  Ast.Const(const_int32 (tag_of_type tag));
  Ast.Binary(Values.I32 Ast.IntOp.Xor);
]

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
    let slot = add_dummy_loc ((env.num_args + (List.length swap_slots)) ++ i) in
    if is_get then
     [Ast.LocalGet(slot)]
    else
     [Ast.LocalSet(slot)]
 | MSwapBind(i) ->
    (* Swap bindings need to be offset to account for arguments *)
    let slot = add_dummy_loc (env.num_args ++ i) in
    if is_get then
      [Ast.LocalGet(slot)]
    else
      [Ast.LocalSet(slot)]
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
      [load ~offset:(Int32.mul 4l (Int32.add 1l i)) ()]
 (* MImport case never needed *)

let get_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then
      raise Not_found;
    compile_bind ~is_get:true env (MSwapBind(Int32.of_int (idx + swap_i32_offset)))
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then
      raise Not_found;
    compile_bind ~is_get:true env (MSwapBind(Int32.of_int (idx + swap_i64_offset)))
  | _ -> raise Not_found

let set_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then
      raise Not_found;
    compile_bind ~is_get:false env (MSwapBind(Int32.of_int (idx + swap_i32_offset)))
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then
      raise Not_found;
    compile_bind ~is_get:false env (MSwapBind(Int32.of_int (idx + swap_i64_offset)))
  | _ -> raise Not_found

let tee_swap ?ty:(typ=Types.I32Type) env idx =
 match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots_i32) then raise Not_found else
    [Ast.LocalTee(add_dummy_loc (Int32.of_int (env.num_args + idx + swap_i32_offset)))]
  | Types.I64Type ->
    if idx > (List.length swap_slots_i64) then raise Not_found else
    [Ast.LocalTee(add_dummy_loc (Int32.of_int (env.num_args + idx + swap_i64_offset)))]
  | _ -> raise Not_found

let compile_imm (env : env) (i : immediate) : Wasm.Ast.instr' list =
  match i with
  | MImmConst c -> compile_const env c
  | MImmBinding b -> compile_bind ~is_get:true env b
  | MImmFail j -> (match j with
    | -1l -> [Ast.Unreachable] (* trap *)
    (* get block to jump to *)
    | _ -> [Ast.Br (add_dummy_loc (List.assoc j env.handler_heights))])


(* call_error_handler left out - not doing proper exceptions (makes a call to runtime_throw_error) *)
(* Don't think error_if_true or check_overflow needed either - OCaml allows slient overflows *)

let compile_unary env op arg : Wasm.Ast.instr' list =
  let compiled_arg = compile_imm env arg in
  match op with
  | UnAdd -> compiled_arg (* Does nothing *)
  | UnNeg -> (Ast.Const(encoded_const_int 0)) :: compiled_arg @ [Ast.Binary(Values.I32 Ast.IntOp.Sub)]
  | Not -> compiled_arg @
      const_true @ (* Flip the bit encoded as true/false *)
      [Ast.Binary(Values.I32 Ast.IntOp.Xor);]
  | Succ -> compiled_arg @ [
      Ast.Const(encoded_const_int 1);
      Ast.Binary(Values.I32 Ast.IntOp.Add);
    ]
  | Pred -> compiled_arg @ [
      Ast.Const(encoded_const_int 1);
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]
  | Abs -> compiled_arg @ [call_abs env]
  (* Skip calling make_float and just create the float constant 0.0 directly *)
  | FUnNeg -> [Ast.Const (add_dummy_loc (Values.F64Value.to_value (Wasm.F64.of_float 0.0)));] @
    compiled_arg @ [load_float; Ast.Binary(Values.F64 Ast.FloatOp.Sub); make_float env]
  | FSqrt -> compiled_arg @ [load_float; Ast.Unary(Values.F64 Ast.FloatOp.Sqrt); make_float env]

(* Assumes all operations are on integers, can't reuse for floats *)
let compile_binary (env : env) op arg1 arg2 : Wasm.Ast.instr' list =
  let compiled_arg1 = compile_imm env arg1 in
  let compiled_arg2 = compile_imm env arg2 in
  let swap_get = get_swap ~ty:Types.I32Type env 0 in
  let swap_tee = tee_swap ~ty:Types.I32Type env 0 in
  (*
  TODO: Not thinking about overflows/31-bit integers initially.
        Work out if overflow_safe needed or not for OCaml
  *)
  match op with
  | Add ->
    (* TODO: Removed overflow_safe bit *)
    (* Removed all casting etc. for now. That and overflow checking may be needed once values tagged
       and treated as 31/63 bit ints. *)
    compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.Add);]
  | Sub ->
    compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.Sub);]
  | Mult ->
    (* Untag one of the numbers:
       ((a * 2) / 2) * (b * 2) = (a * b) * 2
    *)
    compiled_arg1 @ decode_num @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.Mul);]
  | Div -> (* Both div and rem are signed in OCaml *)
    (* (a * 2) / ((b * 2)/2) = (a * b) * 2 *)
     compiled_arg1 @ compiled_arg2 @ decode_num @ [Ast.Binary(Values.I32 Ast.IntOp.DivS);]
  | Mod -> (* Both div and rem are signed in OCaml *)
     compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.RemS);]
  (* Can still occur due to how && and || are compiled when not applied to anything??
     i.e. when they are compiled to function abstractions, still use AND/OR rather than rewriting as an if-then-else *)
  (* Note - safe to recompile args since compile_imm's only side-effect is generating dummy locations *)
  | AND -> (* TODO: Can just use actual And operation? Side-effect semantics removed higher up. This just avoids encoding after *)
    compiled_arg1 @
    swap_tee @
    decode_num @ [
      Ast.If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_loc (compile_imm (enter_block env) arg2), (* Recompile with updated trap handlers *)
             List.map add_dummy_loc swap_get)
    ]
  | OR ->
    compiled_arg1 @
    swap_tee @
    decode_num @ [
      Ast.If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_loc swap_get,
             List.map add_dummy_loc (compile_imm (enter_block env) arg2))
    ]
  | GT ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Ast.Const(const_int32 0); Ast.Compare(Values.I32 Ast.IntOp.GtS)] @ encode_num
  | GTE ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Ast.Const(const_int32 0); Ast.Compare(Values.I32 Ast.IntOp.GeS)] @ encode_num
  | LT ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Ast.Const(const_int32 0); Ast.Compare(Values.I32 Ast.IntOp.LtS)] @ encode_num
  | LTE ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Ast.Const(const_int32 0); Ast.Compare(Values.I32 Ast.IntOp.LeS)] @ encode_num
  | Eq ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Ast.Test(Values.I32 Ast.IntOp.Eqz);] @ encode_num
  | Neq -> (* TODO: Could optimise to use Select? *)
     compiled_arg1 @ compiled_arg2 @
     [call_compare env; Ast.Const(const_int32 0); Ast.Compare(Values.I32 Ast.IntOp.GtU)] @ encode_num
  | Compare -> compiled_arg1 @ compiled_arg2 @ [call_compare env;] (* @ encode_num Not needed - takes difference of encoded args *)
  | Eq_phys -> compiled_arg1 @ compiled_arg2 @ [Ast.Compare(Values.I32 Ast.IntOp.Eq)] @ encode_num
  | Neq_phys -> compiled_arg1 @ compiled_arg2 @ [Ast.Compare(Values.I32 Ast.IntOp.Eq)] @ encode_num @
    const_true @ [Ast.Binary(Values.I32 Ast.IntOp.Xor);] (* Flip the bit encoded as true/false *)
  (* Append currently being mapped to a linast expression higher up, likewise min/max *)
  | Min -> compiled_arg1 @ compiled_arg2 @ [call_min env;]
  | Max -> compiled_arg1 @ compiled_arg2 @ [call_max env;]
  | Append -> compiled_arg1 @ compiled_arg2 @ [call_append env;]
  | FAdd -> compiled_arg1 @ [load_float] @ compiled_arg2 @
    [load_float; Ast.Binary(Values.F64 Ast.FloatOp.Add); make_float env]
  | FSub -> compiled_arg1 @ [load_float] @ compiled_arg2 @
    [load_float; Ast.Binary(Values.F64 Ast.FloatOp.Sub); make_float env]
  | FMult -> compiled_arg1 @ [load_float] @ compiled_arg2 @
    [load_float; Ast.Binary(Values.F64 Ast.FloatOp.Mul); make_float env]
  | FDiv -> compiled_arg1 @ [load_float] @ compiled_arg2 @
    [load_float; Ast.Binary(Values.F64 Ast.FloatOp.Div); make_float env]

(** Heap allocations. *)
let round_up (num : int) (multiple : int) : int =
  multiple * (((num - 1) / multiple) + 1)

let heap_allocate env (num_words : int) =
  [Ast.Const(const_int32 (4 * num_words)); call_alloc env;]

(* Not sure check_memory needed *)
(* Not doing strings initially, so can leave out allocate_string/buf_to_ints *)

(* TODO: Functions allocated by MStore and MAllocate get free vars filled in separately, MStore is more complex process *)
(* Closure represented in memory as [function index, free vars...] *)
let allocate_closure env ?lambda ({func_idx; arity; variables} as closure_data) =
  let num_free_vars = List.length variables in
  let closure_size = num_free_vars + 1 in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  (* TODO: Purpose of this? For a recursive function maybe, putting itself into the allocated closure? *)
  let access_lambda = Option.value ~default:(get_swap @ [
      Ast.Const(const_int32 (4 * closure_size));
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]) lambda in
  env.backpatches := (access_lambda, closure_data)::!(env.backpatches);
  (heap_allocate env closure_size) @ tee_swap @
  [Ast.Const(wrap_int32 (Int32.(add func_idx (of_int env.func_offset)))); store ();]
   @ get_swap @ [
    Ast.Const(const_int32 (tag_of_type Closure)); (* Apply the Lambda tag *)
    Ast.Binary(Values.I32 Ast.IntOp.Or);]

let allocate_data env vtag elts =
  (* Heap memory layout of ADT types:
    [ <variant_tag>, <arity>, elts ... ]
   *)
  let num_elts = List.length elts in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let compile_elt idx elt =
    get_swap @
    (compile_imm env elt) @ [
      store ~offset:(Int32.of_int(4 * (idx + 2))) ();
    ] in
  (heap_allocate env (num_elts + 2)) @ tee_swap @
  [Ast.Const(encoded_const_int32 vtag);
    store ~offset:0l ();
  ] @ get_swap @ [
    Ast.Const(const_int32 num_elts);
    store ~offset:4l ();
  ] @ (List.flatten @@ List.mapi compile_elt elts) @ get_swap
   @ [Ast.Const(const_int32 (tag_of_type Data));
    Ast.Binary(Values.I32 Ast.IntOp.Or);]

let compile_allocation env alloc_type =
  match alloc_type with
  | MClosure(cdata) -> allocate_closure env cdata
  (* | MString(str) -> allocate_string env str *)
  | MData(tag, elts) -> allocate_data env tag elts

let compile_data_op env imm op =
  let block = compile_imm env imm in
  match op with
  | MGet(idx) ->
    block @ (untag Data) @ [
        load ~offset:(Int32.mul 4l (Int32.add idx 2l)) (); (* +2 as blocks start with variant tag; arity; ... *)
      ]
  | MSet(idx, imm) ->
    block @ (untag Data) @ (compile_imm env imm) @ [
        store ~offset:(Int32.mul 4l (Int32.add idx 2l)) ();
      ] @ (compile_imm env imm) (* Why do we put the value on the stack again after? *)
  | MGetTag -> (* Not divided by 2 unless actually used in a switch *)
    block @ (untag Data) @ [
      load ~offset:0l ();
    ]
  | MArrayGet idx ->
    let get_swap = get_swap env 0 in
    let tee_swap = tee_swap env 0 in
    let index = compile_imm env idx in
    (* Currently written to only use 1 swap register, may be easier/efficient with 2? *)
    block @ (untag Data) @ tee_swap @ [load ~offset:0l ();] @
    (* stack is: tag|block|... *)
    index @
    [Ast.Compare(Values.I32 Ast.IntOp.GtS);] @ (* number of element > index *)
    index @ [Ast.Const(wrap_int32 0l); Ast.Compare(Values.I32 Ast.IntOp.GeS);] @ (* index >= 0 *)
    (* stack is: 0<=index|index<tag|block|... *)
     [Ast.Binary(Values.I32 Ast.IntOp.And);
      Ast.If(ValBlockType (Some Types.I32Type),
      (* Calculate address as 4*(idx + 2) = 4*idx + 2 *)
      List.map add_dummy_loc (get_swap @ index @ decode_num @ [
      Ast.Const(wrap_int32 4l); Ast.Binary(Values.I32 Ast.IntOp.Mul);
      Ast.Binary(Values.I32 Ast.IntOp.Add); load ~offset:8l ()]),
      [add_dummy_loc Ast.Unreachable]);]
  | MArraySet (idx, v) ->
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  let index = compile_imm env idx in
  let value = compile_imm (enter_block env) v in
  (* Currently written to only use 1 swap register, may be easier/efficient with 2? *)
  block @ (untag Data) @ tee_swap @ [load ~offset:0l ();] @
  (* stack is: tag|block|... *)
  index @
  [Ast.Compare(Values.I32 Ast.IntOp.GtS);] @ (* number of element > index *)
  index @ [Ast.Const(wrap_int32 0l); Ast.Compare(Values.I32 Ast.IntOp.GeS);] @ (* index >= 0 *)
  (* stack is: 0<=index|index<tag|block|... *)
   [Ast.Binary(Values.I32 Ast.IntOp.And);
    Ast.If(ValBlockType (Some Types.I32Type),
    (* Calculate address as 4*(idx + 2) = 4*idx + 2 *)
    List.map add_dummy_loc (get_swap @ index @ decode_num @ [
    Ast.Const(wrap_int32 4l); Ast.Binary(Values.I32 Ast.IntOp.Mul);   (* Array store returns unit *)
    Ast.Binary(Values.I32 Ast.IntOp.Add);] @ value @ ((store ~offset:8l ()) :: const_false)),
    [add_dummy_loc Ast.Unreachable]);]

(* What is this doing? Appears to just call f on the given env, but with backpatches reset *)
let collect_backpatches env f =
  let nested_backpatches = ref [] in
  let res = f {env with backpatches=nested_backpatches} in
  (* TODO: Work out if original ones wanted or not *)
  res, !nested_backpatches

(* Mutually recursive functions, once closures created, need to associate each variable for one of the other functions
   (or themself) with that closure. So go through and put a pointer to each needed closure in each of the closures. *)
let do_backpatches env backpatches =
  let do_backpatch (lam, {variables}) =
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    (* Put lam in the swap register *)
    let preamble = lam @ (untag Closure) @ set_swap in
    (* TODO: Should skip if nothing to backpatch? *)
    let backpatch_var idx var = (* Store the var as the first free variable of the lambda *)
      get_swap @ (compile_imm env var) @ [store ~offset:(Int32.of_int(4 * (idx + 1))) ();] in
    preamble @ (List.flatten (List.mapi backpatch_var variables)) in
  (List.flatten (List.map do_backpatch backpatches))

(* Used to compile MStore(binds) instructions. Does backpatches (see above) *)
let rec compile_store env binds =
  let process_binds env =
    let process_bind (b, instr) acc =
      let store_bind = compile_bind ~is_get:false env b in
      let get_bind = compile_bind ~is_get:true env b in
      let compiled_instr = match instr with
        | MAllocate(MClosure(cdata)) ->
          (* Special lambda specified since several mutually recursive functions could be getting defined,
             access each one by the identifier it is bound to. *)
          allocate_closure env ~lambda:get_bind cdata
        | _ -> compile_instr env instr in
      (compiled_instr @ store_bind) @ acc in
    List.fold_right process_bind binds [] in
  let instrs, backpatches = collect_backpatches env process_binds in
  instrs @ (do_backpatches env backpatches)

(* TODO: Detect when better to just use nested ifthenelse *)
(* Rewriting Grain version to put one "value" block at top, rest nonetype *)
and compile_switch env arg branches default =
  let compile_table labels =
    let max_label = List.fold_left max 0 labels in
    let labs_to_branches = List.mapi (fun i l -> (l, i+1)) labels in
    let default = add_dummy_loc 0l in
    (* +1 since List.init n creates cases for 0 up to (n-1) *)
    List.init (max_label + 1) (fun l -> match List.assoc_opt l labs_to_branches with
      | Some b -> add_dummy_loc (Int32.of_int b) | None -> default) in
  let rec build_branches i seen = function
    (* Base case, do actual arg eval, branch table and default case *)
    | [] ->
     [Ast.Block(ValBlockType None, (* Only left by branch table *)
        List.map add_dummy_loc ((compile_imm (enter_block ~n:(i + 2) env) arg) @
        decode_num @ [Ast.BrTable (compile_table seen, add_dummy_loc 0l)]))]
        @ (compile_block (enter_block ~n:(i + 1) env) default) @ [Ast.Br(add_dummy_loc (Int32.of_int i))]
    (* Some constructor case, wrap recursive call in this action + jump to end of switch *)
    | (l, action)::rest -> (Ast.Block(ValBlockType None,
     (* TODO: Probably not worth having tags as int32s everywhere if changed to int here.
              Never going to have 2^30 variants. *)
      List.map add_dummy_loc ((build_branches (i+1) ((Int32.to_int l)::seen) rest))))
      :: (compile_block (enter_block ~n:(i+1) env) action) @ [Ast.Br(add_dummy_loc (Int32.of_int i))] in
  [Ast.Block(ValBlockType (Some Types.I32Type), List.map add_dummy_loc (build_branches 0 [] branches))]


and compile_block env block =
  List.flatten (List.map (compile_instr env) block)

(* THE CENTRAL FUNCTION USING ALL THE THINGS ABOVE *)
and compile_instr env instr =
  match instr with
  | MDrop -> [Ast.Drop]
  | MImmediate(imm) -> compile_imm env imm
  | MAllocate(alloc) -> (* New - currying appeared to not work before *)
  let new_backpatches = ref [] in
  let instrs = compile_allocation {env with backpatches=new_backpatches} alloc in
  let do_backpatch (lam, {func_idx;variables}) =
      let get_swap = get_swap env 0 in
      let tee_swap = tee_swap env 0 in
      (* TODO: Should skip if nothing to backpatch? *)
      let backpatch_var idx var = (* Store the var as the first free variable of the lambda *)
        get_swap @ (compile_imm env var) @ [store ~offset:(Int32.of_int(4 * (idx + 1))) ();] in
      (* Takes tag off, puts vars in, puts tag back on. TODO: Reduce number of times tag added/removed *)
      (untag Closure) @ tee_swap @ (List.flatten (List.mapi backpatch_var variables)) @ (untag Closure) in
    (* Inefficient - at most one thing allocated so could use a case split rather than List map/flatten *)
    instrs @ (List.flatten (List.map do_backpatch (!new_backpatches)))

  | MDataOp(op, block) -> compile_data_op env block op
  | MUnary(op, arg) -> compile_unary env op arg
  | MBinary(op, arg1, arg2) -> compile_binary env op arg1 arg2
  | MSwitch(arg, branches, default) -> compile_switch env arg branches default
  | MStore(binds) -> compile_store env binds (* Difference between MAllocate and MStore - alloc for compound, store for toplevel *)
  (* args are curried, so unroll this to do many calls *)
  | MCallIndirect(func, args) ->
    let compiled_func = compile_imm env func in
    let get_swap = get_swap env 0 in
    let tee_swap = tee_swap env 0 in
    List.fold_left
    (fun f arg -> let compiled_arg = compile_imm env arg in
      let ftype = add_dummy_loc (Int32.of_int (get_arity_func_type_idx env 2)) in
      f @ (untag Closure) @ tee_swap @ compiled_arg @ get_swap @ [load ~offset:0l (); Ast.CallIndirect(ftype);])
    compiled_func args

  | MIf(cond, thn, els) ->
    let compiled_cond = compile_imm env cond in
    let compiled_thn = List.map
        add_dummy_loc
        (compile_block (enter_block env) thn) in
    let compiled_els = List.map
        add_dummy_loc
        (compile_block (enter_block env) els) in
    compiled_cond @
    (* TODO: Can remove decode steps here since 2=1=true and 0=false *)
    decode_num @ [
      Ast.If(ValBlockType (Some Types.I32Type),
             compiled_thn,
             compiled_els);
    ]

  | MWhile(cond, body) ->
    let compiled_cond = compile_block (enter_block ~n:2 env) cond in
    let compiled_body = (compile_block (enter_block ~n:2 env) body) in
    [Ast.Block(ValBlockType (Some Types.I32Type),
       List.map add_dummy_loc
        [Ast.Loop(ValBlockType (Some Types.I32Type),
              List.map add_dummy_loc
              (const_false @
              compiled_cond @
              decode_num @
              [Ast.Test(Values.I32 Ast.IntOp.Eqz); (* TODO: Why the extra test? Checking for NOT true? *)
               Ast.BrIf (add_dummy_loc @@ Int32.of_int 1)] @
              [Ast.Drop] @
              compiled_body @
              [Ast.Br (add_dummy_loc @@ Int32.of_int 0)]))])]

  | MFor(arg, start_expr, direction, end_arg, end_expr, body) ->
    let compiled_start = compile_imm env start_expr in
    let compiled_end = compile_imm env end_expr in
    let compiled_body = (compile_block (enter_block ~n:2 env) body) in
    compiled_start @ (compile_bind ~is_get:false env arg) @
    compiled_end @ (compile_bind ~is_get:false env end_arg) @
    [Ast.Block(ValBlockType (Some Types.I32Type),
       List.map add_dummy_loc
        [Ast.Loop(ValBlockType (Some Types.I32Type),
              List.map add_dummy_loc
              (const_false @ (* Return unit value when loop fails *)
              (compile_bind ~is_get:true env arg) @
              (compile_bind ~is_get:true env end_arg) @
              [Ast.Compare(Values.I32
                (match direction with Upto -> Ast.IntOp.GtS | Downto -> Ast.IntOp.LtS))] @
              [Ast.BrIf (add_dummy_loc @@ Int32.of_int 1)] @
              compiled_body @
              (compile_bind ~is_get:true env arg) @
              [Ast.Const(encoded_const_int 1); (* For loop actually takes steps of 2 due to encoding *)
               Ast.Binary(Values.I32
                 (match direction with Upto -> Ast.IntOp.Add | Downto -> Ast.IntOp.Sub));] @
              (compile_bind ~is_get:false env arg) @ (* TODO: Could use Tee here? Avoiding 'get' at top *)
              [Ast.Br (add_dummy_loc @@ Int32.of_int 0)]))])]

  (* Creates two blocks. Inner block is usual 'try' body, outer block is that + handler body.
     If try case succeeds, Br 1 jumps to the end of the outer block so just returns result.
     Fail's within the body map to a branch to the end of the inner block, so run the handler.
     TODO: Check semantics - usual blocks return a value hence Some type. Handler should discard so is None type?
           But While loop above has the Loop block with Some type, yet that doesn't take/return anything?? *)
  | MTry(i, body, handler) ->
    let body_env = enter_block ~n:2 env in
    let compiled_body = compile_block {body_env with handler_heights = (i,0l)::body_env.handler_heights} body in
    let handler_body = compile_block (enter_block env) handler in
    [Ast.Block(ValBlockType (Some Types.I32Type), (* Outer 'try/with' block, returns result *)
       List.map add_dummy_loc
        ([Ast.Block(ValBlockType None, (* inner block for body - only left by fail *)
              List.map add_dummy_loc
              (compiled_body @ [Ast.Br (add_dummy_loc 1l)]))]  (* try case succeeded, skip handler *)
        @ handler_body))]

  (* Not actually used? *)
  | MCallKnown(func_idx, args) ->
    let compiled_args = List.flatten @@ List.map (compile_imm env) args in
    compiled_args @ [
       Ast.Call(add_dummy_loc
         (Int32.of_int ((* env.import_func_offset +  -- TODO: should be fixed value *) (Int32.to_int func_idx))));
    ]
  (* TODO: If never compiled, why does it exist? Purely documentation? *)
  | MArityOp _ -> failwith "NYI: (compile_instr): MArityOp"
  | MTagOp _ -> failwith "NYI: (compile_instr): MTagOp"

(* TODO: Understand how arity relates to currying/partial application *)
(* TODO: Args in Grain correspond to elements of a tuple, not curried arguments.
         Either need to change to be a set of functions or have application modify arity until 0, then call. *)
(* For now can always take safe approach and uncurry when compiling to wasmtree *)
let compile_function env {index; arity; stack_size; body=body_instrs} =
  let arity_int = Int32.to_int arity in
  let body_env = {env with num_args=arity_int} in
  let body = List.map add_dummy_loc
    ((compile_block body_env body_instrs) @ [Ast.Return]) in
  let ftype_idx = get_arity_func_type_idx env arity_int in
  let ftype = add_dummy_loc Int32.(of_int ftype_idx) in
  let locals = List.append swap_slots @@ List.init (stack_size) (fun n -> Types.I32Type) in
  let open Wasm.Ast in (* so func' record type in scope *)
  add_dummy_loc {
    ftype;
    locals;
    body;
  }

(* TODO: Is this necessary? (global)Imports should be fixed. Relates to how compile_globals works
   Shouldn't actually import any global costnats, so +2 from grain version removed (grain runtime has 2 globals in it) *)
let compute_table_size env {functions} =
  (List.length functions) + (List.length runtime_imports)

(* TODO: Should be able to massively simplify this. Set of imports should be fixed, ignore any that aren't OcamlRuntime *)
(* TODO: Understand what all of ths does/is needed for *)
let compile_imports env =
  let compile_import {mimp_name; mimp_type} =
    let module_name = encode_string (Ident.name runtime_mod) in
    let item_name = encode_string (Ident.name mimp_name) in
    let idesc = match mimp_type with
      (* TODO: Should this actually be the other case? i.e. line 877 of Grain, not 869.
               Look at what determines ImportGrain vs ImportWasm i.e. which is the runtime environment for Grain *)
      | MGlobalImport typ ->
        let func_type = Types.FuncType([], [typ]) in
        add_dummy_loc @@ Ast.FuncImport(add_dummy_loc @@ Int32.of_int @@ get_func_type_idx env func_type)
      | MFuncImport(args, ret) ->
        let func_type = Types.FuncType(args, ret) in
        add_dummy_loc @@ Ast.FuncImport(add_dummy_loc @@ Int32.of_int @@ get_func_type_idx env func_type)
    in
    (* Wasm.Ast import' type *)
    let open Wasm.Ast in
    add_dummy_loc {
      module_name;
      item_name;
      idesc;
    } in
  let imports = List.map compile_import runtime_imports in
  (List.append
    imports
    (* Single memory/table required by a Wasm module -- imported rather than created itself? *)
    [
      add_dummy_loc {
        Ast.module_name=encode_string (Ident.name runtime_mod);
        Ast.item_name=encode_string "mem";
        Ast.idesc=add_dummy_loc (Ast.MemoryImport (Types.MemoryType({
            Types.min=Int32.zero;
            Types.max=None;
          })));
      };
    ])

(* Is there any need for naming extensions? Doesn't look like it except to avoid naming something _start etc.
   For now assume that never happens - convenience of being able to use actual names vs *)
(* Linearise ensures that only one thing with each name gets exported, so don't need worry about duplicates *)
(* TODO: Modify getter functions to do decode? Or just do in runtime/JS caller *)
let compile_exports env {functions; exports; num_globals} =
  (* TODO: What are these indexes? *)
  (* `Getter` provides a simple way to access exported varaibles - provides a nullary function that just
     returns the corresponding global variable. (Function name matches name of variable)*)
  let compile_getter i {ex_name; ex_global_index; ex_getter_index} =
    let exported_name = (*"GRAIN$EXPORT$GET$" ^*) (Ident.name ex_name) in
    let name = encode_string exported_name in
    let export =
      let open Wasm.Ast in
      add_dummy_loc {
        name;
        edesc=add_dummy_loc (Ast.FuncExport (add_dummy_loc (Int32.add (Int32.of_int env.func_offset) ex_getter_index)));
      } in
    export
  in
  (* Exports every function in the program, allows partially applied functions to be returned.
     Runtime functions not exported so integer used for name is offset to align with function index.
     i.e. export "i" is function i. No clashes as variable names can't start with digits *)
  let compile_lambda_export i _ =
    let name = encode_string ((* "GRAIN$LAM_" ^  -- also not needed? *) (string_of_int (i + env.func_offset))) in
    let edesc = add_dummy_loc (Ast.FuncExport(add_dummy_loc @@ Int32.of_int (i + env.func_offset))) in
    let open Wasm.Ast in
    add_dummy_loc { name; edesc } in
  let heap_adjust_idx = env.func_offset + (List.length functions) in
  let main_idx = heap_adjust_idx in
  let main_idx = add_dummy_loc (Int32.of_int main_idx) in
  (* Make each function visible outside of module *)
  let compiled_lambda_exports = List.mapi compile_lambda_export functions in
  (* Export the varaibles/functions declared in the program using their actual names *)
  let compiled_exports = List.mapi compile_getter exports in
     compiled_lambda_exports @
        compiled_exports @
        [
          add_dummy_loc {
            Ast.name=encode_string "OCAML$MAIN";
            Ast.edesc=add_dummy_loc (Ast.FuncExport main_idx);
          }; (* Export the memory - makes JS wrapper neater *)
         add_dummy_loc {
            Ast.name=encode_string "$mem";
            Ast.edesc=add_dummy_loc (Ast.MemoryExport (add_dummy_loc 0l)); (* Index of the only memory *)
            };
        ]

let compile_elems env prog =
  let table_size = compute_table_size env prog in
  let open Wasm.Ast in
  (* Elems initialises the function table, just initialise ith element of table to point to function i (last line) *)
  (* https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format - Defining a table in wasm *)
  [
    add_dummy_loc {
      index=add_dummy_loc (Int32.zero);
      offset=add_dummy_loc [
        add_dummy_loc (Ast.Const(const_int32 0));
      ];
      init=List.init table_size (fun n -> (add_dummy_loc (Int32.of_int n)));
    };
  ]
let compile_globals env {num_globals} =
    (List.init num_globals (fun _ -> add_dummy_loc {
         Ast.gtype=Types.GlobalType(Types.I32Type, Types.Mutable);
         Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 0)]);
       })) (* No need to store the table size, externally use the exported functions, not the table *)

let compile_main env prog =
  compile_function env
    {
      index=Int32.of_int (-99); (* Not needed at this level, used in previous stage to assign function indices *)
      arity=Int32.zero; (* This means local.get/set aren't wrong due to no args at start of locals *)
      body=prog.main_body; (* top part of program put into its own function. Main effectively acts as _start? *)
      stack_size=prog.main_body_stack_size;
    }

let compile_functions env ({functions} as prog) =
  let compiled_funcs = List.map (compile_function env) functions in
  let main = compile_main env prog in
    compiled_funcs @ [main]

let module_to_string compiled_module =
  (* Print module to string *)
  Wasm.Sexpr.to_string 80 @@ Wasm.Arrange.module_ compiled_module

let reparse_module (module_ : Wasm.Ast.module_) =
  let open Wasm.Source in
  let as_str = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ module_) in
  let {it=script} = Wasm.Parse.string_to_module as_str in
  match script with
  | Wasm.Script.Textual(m) -> m
  | Encoded _ -> failwith "Internal error: reparse_module: Returned Encoded (should be impossible)"
  | Quoted _ -> failwith "Internal error: reparse_module: Returned Quoted (should be impossible)"

(* Leaving out "reparse" function for now - meant so that actual locations of parts of program can be found
   when validation fails. TODO: May want to add this to help with debugging *)
exception WasmRunnerError of Wasm.Source.region * string * Wasm.Ast.module_
let validate_module (module_ : Wasm.Ast.module_) =
 try
    Valid.check_module module_
  with
  | Wasm.Valid.Invalid(region, msg) ->
    (* Re-parse module in order to get actual locations *)
    let reparsed = reparse_module module_ in
    (try
       Valid.check_module reparsed
     with
     | Wasm.Valid.Invalid(region, msg) ->
       raise (WasmRunnerError(region, msg, reparsed)));
    raise (WasmRunnerError(region, Printf.sprintf "WARNING: Did not re-raise after reparse: %s" msg, module_))

(* Fold left but with an added integer argument *)
let rec fold_lefti f e l =
  fst (List.fold_left (fun (e, i) x -> (f e i x, i+1)) (e, 0) l)

(* Sets up 'env' with all the imported runtime functions *)
let prepare env =
  List.iteri (fun idx {mimp_name; mimp_type;} -> Hashtbl.add imported_funcs mimp_name (Int32.of_int idx)) runtime_imports;
  {env with func_offset=List.length runtime_imports;}

let compile_wasm_module prog =
  let open Wasm.Ast in
  let env = prepare init_env in
  let funcs = compile_functions env prog in
  let imports = compile_imports env in
  let exports = compile_exports env prog in
  let globals = compile_globals env prog in
  let elems = compile_elems env prog in
  let types = List.map add_dummy_loc (!(env.func_types)) in
  let ret = add_dummy_loc {
    empty_module with
    funcs;
    imports;
    exports;
    globals;
    (* Create a function table large enough to hold pointers to each function in the program *)
    tables=[add_dummy_loc {
      ttype = TableType({min=Int32.of_int(compute_table_size env prog); max=None}, FuncRefType)}];
    elems;
    types;
    (* We don't set start to be the main function since start must have type () -> (), but OCaml programs
       can evaluate to a value e.g. '10' is a valid program. *)
    start=None;
  } in
  validate_module ret;
  ret

let _ = (* Registers an exception printer to print out module if an exception occurs *)
  Printexc.register_printer (fun exc ->
      match exc with
      | WasmRunnerError(region, str, module_) ->
        let fmt_module _ m = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ m) in
        let s = Printf.sprintf "WASM Runner Exception at %s: '%s'\n%a\n"
          (Wasm.Source.string_of_region region) str
          fmt_module module_ in
        Some(s)
      | _ -> None)