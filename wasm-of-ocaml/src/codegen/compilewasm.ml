open Bindstree
open Wasm
open Graph
(* TODO: Use concatList to enable pre/post append efficiently *)
(* deque package: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatDeque.html
   and similar lists: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatList.html *)

open Compilerflags

(* Needed as Wasm Ast represents module names (e.g. in import list) as "name" type, which is actually int list *)
let encode_string : string -> int list = Utf8.decode

(* Since only imports are the runtime functions (no variables from other modules), all imports are functions
   so also don't need to put them into global variables, can just track the function index offset and nothing else. *)
type env = {
  num_args: int;
  func_offset: int;
  (*  OCaml runtime should only contain functions, so don't need to track globals *)
  (* No modules so only imports are from runtime functions, don't need to remember offset of imports *)
 func_types : Wasm.Types.func_type list ref;
  (* Allocated closures which need backpatching *)
  backpatches: (instr' (* Concatlist.t  *) list * closure_data) list ref;
  (* Number of blocks to jump through to reach each handler in scope.
     Possibly better choices to use than lists but never mind. *)
  handler_heights: (int32 * int32) list;
}

let enter_block ?(n=1) ({handler_heights;} as env) =
  {env with handler_heights = List.map (fun (handler, height) -> (handler, Int32.add (Int32.of_int n) height)) handler_heights}

(* Number of swap variables to allocate *)
(* Swap varaibles are only ever int32s, so no type argument to get/set swap *)
let swap_slots = [Types.I32Type; Types.I32Type]

let runtime_mod = Ident.create_persistent "ocamlRuntime"
let malloc_ident = Ident.create_persistent "malloc"
let compare_ident = Ident.create_persistent "compare"
let abs_ident = Ident.create_persistent "abs"
let min_ident = Ident.create_persistent "min"
let max_ident = Ident.create_persistent "max"
let append_ident = Ident.create_persistent "@"
let make_float_ident = Ident.create_persistent "make_float"
(* For garbage collection *)
let create_fun_ident = Ident.create_persistent "create_fun"
let exit_fun_ident = Ident.create_persistent "exit_fun"
let update_local_ident = Ident.create_persistent "update_local"

(* Runtime should only import functions, no globals, so only need to track offset due to functions *)
(* Function needed to delay evaluation of no_gc *)
let runtime_imports _ =
  (if !no_gc then [] else
   [{ mimp_name=create_fun_ident; mimp_type=([I32Type], []); };
    { mimp_name=exit_fun_ident; mimp_type=([I32Type], []); };
    { mimp_name=update_local_ident; mimp_type=([I32Type; I32Type], [I32Type]); };])
  @
  [{ mimp_name=malloc_ident; mimp_type=([I32Type], [I32Type]); };
  { mimp_name=compare_ident; mimp_type=([I32Type; I32Type], [I32Type]); };
  { mimp_name=abs_ident; mimp_type=([I32Type], [I32Type]); };
  { mimp_name=min_ident; mimp_type=([I32Type; I32Type], [I32Type]); };
  { mimp_name=max_ident; mimp_type=([I32Type; I32Type], [I32Type]); };
  { mimp_name=append_ident; mimp_type=([I32Type; I32Type], [I32Type]); };
  { mimp_name=make_float_ident; mimp_type=([F64Type], [I32Type]); };]

let imported_funcs : (Ident.t, int32) Hashtbl.t = Hashtbl.create (List.length (runtime_imports ()))

let init_env = {
  num_args=0;
  func_offset=0;
  func_types=ref [];
  backpatches=ref [];
  handler_heights = [];
}

(* Finds the function number each runtime import was bound to during setup *)
let lookup_runtime_func itemname =
  Hashtbl.find imported_funcs itemname
let var_of_runtime_func itemname =
  add_dummy_loc (lookup_runtime_func itemname)
let call_malloc env = Call(var_of_runtime_func malloc_ident)
let call_compare env = Call(var_of_runtime_func compare_ident)
let call_abs env = Call(var_of_runtime_func abs_ident)
let call_min env = Call(var_of_runtime_func min_ident)
let call_max env = Call(var_of_runtime_func max_ident)
let call_append env = Call(var_of_runtime_func append_ident)
let make_float env = Call(var_of_runtime_func make_float_ident)

(* TODO: Support strings *)

let const_int32 n = add_dummy_loc (Values.I32Value.to_value (Int32.of_int n))
let const_int64 n = add_dummy_loc (Values.I64Value.to_value (Int64.of_int n))
let const_float64 n = add_dummy_loc (Values.F64Value.to_value (Wasm.F64.of_float n))

(* These are like the above 'const' functions, but take inputs
   of the underlying types instead *)
let wrap_int32 n = add_dummy_loc (Values.I32Value.to_value n)
let wrap_int64 n = add_dummy_loc (Values.I64Value.to_value n)
let wrap_float64 env n = [Const (add_dummy_loc (Values.F64Value.to_value n)); make_float env]

(* For integers taken out of wasmtree - all tags/ints need doubling (but not memory offsets) *)
let encoded_int n = n * 2
let encoded_int32 n = Int32.mul 2l n
let encoded_const_int n = const_int32 (encoded_int n)
let encoded_const_int32 n = wrap_int32 (encoded_int32 n)
(** Constant compilation *)
let rec compile_const env c =
    match c with
    | MConstI32 n -> [Const(encoded_const_int32 n)]
    | MConstI64 n -> failwith "64-bit ints not supported"
    | MConstF64 n -> wrap_float64 env (Wasm.F64.of_float n)

(* Translate constants to WASM. Override names from wasmtree to be wasm phrases (values with regions) *)
let const_true = compile_const init_env const_true
let const_false = compile_const init_env const_false (* also equals unit i.e. () *)

(* WebAssembly helpers *)
let store
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0l)
    ?sz:(sz=None)
    () =
  let open Graph in
  Store({ty; align; sz; offset;})

let load
    ?ty:(ty=Wasm.Types.I32Type)
    ?align:(align=2)
    ?offset:(offset=0l)
    ?sz:(sz=None)
    () =
  let open Graph in
  Load({ty; align; sz; offset;})

(* For version with GC: floats are [-1, arity=0, value] so all things can be GC'd in same way
   For non-GC version, leave out arity field as it has no use
*)
(* Offset of 8, floats have a tag of 01, so +7 overall without untagging *)
let load_float _ =
  load ~ty:Wasm.Types.F64Type ~offset:(if !no_gc then 3l else 7l) ()

(* Equivalent to BatDeque.find but on lists *)
let find_index p l =
  let rec aux n = function [] -> None | x::xs -> if p x then Some(n, x) else aux (n+1) xs in
  aux 0 l

(* Rewritten to not bother with a BatDeque *)
let get_func_type_idx env typ =
  match find_index ((=) typ) !(env.func_types) with
  | None ->
    env.func_types := !(env.func_types) @ [typ];
    List.length !(env.func_types) - 1
  | Some((i, _)) -> i

(* All functions are i32 i32 -> i32 except main, which is () -> i32 *)
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
let encode_num = [Const(const_int32 1); Binary(Values.I32 Ast.IntOp.Shl)]
let decode_num = [Const(const_int32 1); Binary(Values.I32 Ast.IntOp.ShrS)]

(* Xor, so can tag/untag a value *)
let toggle_tag tag = [
  Const(const_int32 (tag_of_type tag));
  Binary(Values.I32 Ast.IntOp.Xor);
]

(* Locals in a function are ordered as [arguments (first is closure), swap locals, locals] *)
let compile_bind ~is_get (env : env) (b : binding) : instr' list =
  let (++) a b = Int32.(add (of_int a) b) in
  match b with
  | MArgBind(i) ->
    (* No adjustments are needed for argument bindings *)
    let slot = add_dummy_loc i in
    if is_get then
     [LocalGet(slot)]
    else
      (* May be used once LVA register colouring added,
         but don't need to initialise in advance on shadow-stack at start of function call *)
     [LocalSet(slot)]
  | MLocalBind(i) ->
    (* Local bindings need to be offset to account for arguments and swap variables *)
    let slot = add_dummy_loc ((env.num_args + (List.length swap_slots)) ++ i) in
    if is_get then
     [LocalGet(slot)]
    else
     [LocalSet(slot)]
 | MSwapBind(i) ->
    (* Swap bindings need to be offset to account for arguments *)
    let slot = add_dummy_loc (env.num_args ++ i) in
    if is_get then
      [LocalGet(slot)]
    else
     [LocalSet(slot)]
  | MGlobalBind(i) ->
    (* Global bindings need to be offset to account for any imports *)
    let slot = add_dummy_loc i in
    if is_get then
      [GlobalGet(slot)]
    else
      (* Used to initialise globals, and tracking shared mutable varaibles next/continue/result for TCO *)
     [GlobalSet(slot)]
  | MClosureBind(i) ->
    (* Closure bindings need to be calculated *)
    begin
      if not(is_get) then
        failwith "Internal error: attempted to emit instruction which would mutate closure contents"
    end;
      (* Add 2 since closure is now [func idx; arity; elements] *)
      [LocalGet(add_dummy_loc Int32.zero); load ~offset:(Int32.mul 4l (Int32.add 2l i)) ()]

let get_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots) then
      raise Not_found;
    compile_bind ~is_get:true env (MSwapBind(Int32.of_int idx))
  | _ -> raise Not_found (* Will get an error indicating need to add extra swap slots if other types used in future *)

(* On both this and tee_swap, we skip modifying reference counts as swap locals aren't considered references *)
let set_swap ?ty:(typ=Types.I32Type) env idx =
  match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots) then
      raise Not_found;
    compile_bind ~is_get:false env (MSwapBind(Int32.of_int idx))
  | _ -> raise Not_found

let tee_swap ?ty:(typ=Types.I32Type) env idx =
 match typ with
  | Types.I32Type ->
    if idx > (List.length swap_slots) then raise Not_found else
    [LocalTee(add_dummy_loc (Int32.of_int (env.num_args + idx)))]
  | _ -> raise Not_found

let compile_imm (env : env) (i : immediate) : instr' list =
  match i with
  | MImmConst c -> compile_const env c
  | MImmBinding b -> compile_bind ~is_get:true env b

let compile_unary env op arg : instr' list =
  let compiled_arg = compile_imm env arg in
  match op with
  | UnAdd -> compiled_arg (* Does nothing *)
  | UnNeg -> (Const(encoded_const_int 0)) :: compiled_arg @ [Binary(Values.I32 Ast.IntOp.Sub)]
  | Not -> compiled_arg @
      const_true @ (* Flip the bit encoded as true/false *)
      [Binary(Values.I32 Ast.IntOp.Xor);]
  | Succ -> compiled_arg @ [
      Const(encoded_const_int 1);
      Binary(Values.I32 Ast.IntOp.Add);
    ]
  | Pred -> compiled_arg @ [
      Const(encoded_const_int 1);
      Binary(Values.I32 Ast.IntOp.Sub);
    ]
  | Abs -> compiled_arg @ [call_abs env]
  (* Skip calling make_float and just create the float constant 0.0 directly *)
  | FUnNeg -> [Const (add_dummy_loc (Values.F64Value.to_value (Wasm.F64.of_float 0.0)));] @
    compiled_arg @ [load_float (); Binary(Values.F64 Ast.FloatOp.Sub); make_float env]
  | FSqrt -> compiled_arg @ [load_float (); Unary(Values.F64 Ast.FloatOp.Sqrt); make_float env]

(* Assumes all operations are on integers, can't reuse for floats *)
let compile_binary (env : env) op arg1 arg2 : instr' list =
  let compiled_arg1 = compile_imm env arg1 in
  let compiled_arg2 = compile_imm env arg2 in
  let swap_get = get_swap ~ty:Types.I32Type env 0 in
  let swap_tee = tee_swap ~ty:Types.I32Type env 0 in
  match op with
  | Add ->
    compiled_arg1 @ compiled_arg2 @ [Binary(Values.I32 Ast.IntOp.Add);]
  | Sub ->
    compiled_arg1 @ compiled_arg2 @ [Binary(Values.I32 Ast.IntOp.Sub);]
  | Mult ->
    (* Untag one of the numbers:
       ((a * 2) / 2) * (b * 2) = (a * b) * 2
    *)
    compiled_arg1 @ decode_num @ compiled_arg2 @ [Binary(Values.I32 Ast.IntOp.Mul);]
  | Div -> (* Both div and rem are signed in OCaml *)
    (* (a * 2) / ((b * 2)/2) = (a * b) * 2 *)
     compiled_arg1 @ compiled_arg2 @ decode_num @ [Binary(Values.I32 Ast.IntOp.DivS);]
  | Mod -> (* Both div and rem are signed in OCaml *)
     compiled_arg1 @ compiled_arg2 @ [Binary(Values.I32 Ast.IntOp.RemS);]
  (* Can still occur due to how && and || are compiled when not applied to anything
     i.e. when they are compiled to function abstractions, still use AND/OR rather than rewriting as an if-then-else *)
  (* Note - safe to recompile args since compile_imm's only side-effect is generating dummy locations *)
  | AND -> (* TODO: Can just use actual And operation? *)
    compiled_arg1 @
    swap_tee @
    decode_num @ [
      If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_edges
             (compile_imm (enter_block env) arg2), (* Recompile with updated trap handlers *)
             List.map add_dummy_edges swap_get)
    ]
  | OR ->
    compiled_arg1 @
    swap_tee @
    decode_num @ [
      If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_edges swap_get,
             List.map add_dummy_edges (compile_imm (enter_block env) arg2))
    ]
  | GT ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Const(const_int32 0); Compare(Values.I32 Ast.IntOp.GtS)] @ encode_num
  | GTE ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Const(const_int32 0); Compare(Values.I32 Ast.IntOp.GeS)] @ encode_num
  | LT ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Const(const_int32 0); Compare(Values.I32 Ast.IntOp.LtS)] @ encode_num
  | LTE ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Const(const_int32 0); Compare(Values.I32 Ast.IntOp.LeS)] @ encode_num
  | Eq ->
    compiled_arg1 @ compiled_arg2 @
    [call_compare env; Test(Values.I32 Ast.IntOp.Eqz);] @ encode_num
  | Neq ->
     compiled_arg1 @ compiled_arg2 @
     [call_compare env; Const(const_int32 0); Compare(Values.I32 Ast.IntOp.GtU)] @ encode_num
  | Compare -> compiled_arg1 @ compiled_arg2 @ [call_compare env;] (* @ encode_num Not needed - takes difference of encoded args *)
  | Eq_phys -> compiled_arg1 @ compiled_arg2 @ [Compare(Values.I32 Ast.IntOp.Eq)] @ encode_num
  | Neq_phys -> compiled_arg1 @ compiled_arg2 @ [Compare(Values.I32 Ast.IntOp.Eq)] @ encode_num @
    const_true @ [Binary(Values.I32 Ast.IntOp.Xor);] (* Flip the bit encoded as true/false *)
  (* Append currently being mapped to a linast expression higher up, likewise min/max *)
  | Min -> compiled_arg1 @ compiled_arg2 @ [call_min env;]
  | Max -> compiled_arg1 @ compiled_arg2 @ [call_max env;]
  | Append -> compiled_arg1 @ compiled_arg2 @ [call_append env;]
  | FAdd -> compiled_arg1 @ [load_float ()] @ compiled_arg2 @
    [load_float (); Binary(Values.F64 Ast.FloatOp.Add); make_float env]
  | FSub -> compiled_arg1 @ [load_float ()] @ compiled_arg2 @
    [load_float (); Binary(Values.F64 Ast.FloatOp.Sub); make_float env]
  | FMult -> compiled_arg1 @ [load_float ()] @ compiled_arg2 @
    [load_float (); Binary(Values.F64 Ast.FloatOp.Mul); make_float env]
  | FDiv -> compiled_arg1 @ [load_float ()] @ compiled_arg2 @
    [load_float (); Binary(Values.F64 Ast.FloatOp.Div); make_float env]

(** Heap allocations. *)
let round_up (num : int) (multiple : int) : int =
  multiple * (((num - 1) / multiple) + 1)

let heap_allocate env (num_words : int) =
  [Const(const_int32 (4 * num_words)); call_malloc env;]

(* Not sure check_memory needed *)
(* Not doing strings initially, so can leave out allocate_string/buf_to_ints *)

(* Functions allocated by MStore and MAllocate get free vars filled in separately,
   Can take advantage of information known when MStore happening (know the variable being bound to) *)
(* Closure represented in memory as [function index, free vars...] *)
(* bound_to stores the instructions used to access the variable this function is being assigned to for MStore.
   For plain MAllocate (i.e. function is being returned, so never stored in a variable),
   that just uses get_swap 0 and leaves bound_to as [] *)
let allocate_closure env ?(bound_to=[]) ({func_idx; arity; variables} as closure_data) =
  let num_free_vars = List.length variables in
  let closure_size = num_free_vars + 2 in
  let get_swap = get_swap env 0 in
  let tee_swap = tee_swap env 0 in
  env.backpatches := (bound_to, closure_data)::!(env.backpatches);
  (heap_allocate env closure_size) @ tee_swap @
  [Const(wrap_int32 (Int32.(add func_idx (of_int env.func_offset)))); store ();] (* function index *)
   (* Store number of variables *)
   @ get_swap @ [Const(const_int32 num_free_vars); store ~offset:4l ();]
   @ get_swap @ (toggle_tag Closure) (* Xor, so sets the tag rather than removing it *)

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
  [Const(encoded_const_int32 vtag); (* Tag stored literally, not doubled *)
    store ~offset:0l ();
  ] @ get_swap @ [
    Const(const_int32 num_elts);
    store ~offset:4l ();
  ] @ (List.flatten @@ List.mapi compile_elt elts) @ get_swap
   @ (toggle_tag Data)

let compile_allocation env alloc_type =
  match alloc_type with
  | MClosure(cdata) -> allocate_closure env cdata
  (* | MString(str) -> allocate_string env str *)
  | MData(tag, elts) -> allocate_data env tag elts

let compile_data_op env imm op =
  let block = compile_imm env imm in
  (* For ArrayGet/Set, check index is valid and then do action (if valid). Reduces code duplication *)
  (* action = [load ~offset:8l ()] or value @ ((store ~offset:8l ()) :: const_false) *)
  let check_array_bounds idx action =
    let swap_get = get_swap env 0  and swap_tee = tee_swap env 0
    and index_swap_get = get_swap env 1 and index_swap_tee = tee_swap env 1
    and index = compile_imm env idx in
    block @ (toggle_tag Data) @ swap_tee @ [load ~offset:0l ();] @
    index @ index_swap_tee @
    (* stack is: index|tag|... *)
    (* Array size is actually a 31-bit int so can use unsigned test to safely check 0 <= index too *)
    [Compare(Values.I32 Ast.IntOp.GtU); (* 0 <= index < number of element *)
    If(ValBlockType (Some Types.I32Type),
    List.map add_dummy_edges
    (* Calculate address as 4*(idx + 2) = 4*idx + 2 *)
    (swap_get @ index_swap_get @ decode_num @ [
    Const(wrap_int32 4l); Binary(Values.I32 Ast.IntOp.Mul);
    Binary(Values.I32 Ast.IntOp.Add)] @ action),
    [add_dummy_edges Unreachable]);] in

  match op with
  | MGet(idx) ->
    block @ (toggle_tag Data) @ [
        load ~offset:(Int32.mul 4l (Int32.add idx 2l)) (); (* +2 as blocks start with variant tag; arity; ... *)
      ]
  | MSet(idx, imm) ->
    block @ (toggle_tag Data) @ (compile_imm env imm) @ [
        store ~offset:(Int32.mul 4l (Int32.add idx 2l)) ();
      ] @ const_false (* Return unit *)
  | MGetTag -> (* Not divided by 2 unless actually used in a switch *)
    block @ (toggle_tag Data) @ [
      load ~offset:0l ();
    ]
  | MArrayGet idx -> check_array_bounds idx [load ~offset:8l ()]
  | MArraySet (idx, v) ->
    let value = compile_imm (enter_block env) v in
    check_array_bounds idx (value @ ((store ~offset:8l ()) :: const_false))
  | MAssign b ->
    (* block is actually just a local variable in this case *)
    block @ (compile_bind ~is_get:false env b) @ const_false

let collect_backpatches env f =
  let res = f env in
  let backpatching = !(env.backpatches) in
  env.backpatches := []; (* clear, ready for next function *)
  res, backpatching

(* Mutually recursive functions, once closures created, need to associate each variable for one of the other functions
   (or themself) with that closure. So go through and put a pointer to each needed closure in each of the closures. *)
let do_backpatches env backpatches =
  let do_backpatch (lam, {variables}) =
    (* skip if nothing to backpatch *)
    if variables = [] then [] else
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    (* Put lam in the swap register *)
    let preamble = lam @ (toggle_tag Closure) @ set_swap in
    let backpatch_var idx var = (* Store each free variable in the closure *)
      get_swap @ (compile_imm env var) @ [store ~offset:(Int32.of_int(4 * (idx + 2))) ();] in
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
        (* bound_to is which variable the closure produced is assigned to, so can get it for backpatching later *)
          allocate_closure env ~bound_to:get_bind cdata
        | _ -> compile_instr env instr in
      (compiled_instr @ store_bind) @ acc in
    List.fold_right process_bind binds [] in
  let instrs, backpatches = collect_backpatches env process_binds in
  instrs @ (do_backpatches env backpatches)

and compile_switch env arg branches default =
  let max_label = Int32.to_int (List.fold_left (fun m (i, _) -> max m i) 0l branches) in
  let num_cases = List.length branches in
  if  max_label > 2*num_cases
  then (* Too sparse, use chain of if-then-else statements rather than branch table *)
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    let compiled_arg = compile_imm env arg in
    let rec build_branches i = function
      | [] ->
       (compile_block (enter_block ~n:i env) default) @ [Br(add_dummy_loc (Int32.of_int i))]
      (* Some constructor case, wrap recursive call in this action + jump to end of switch *)
      | (l, action)::rest -> get_swap @ [Const(encoded_const_int32 l); Compare(Values.I32 Ast.IntOp.Eq)] @
       [If(ValBlockType (Some Types.I32Type),
         List.map add_dummy_edges ((compile_block (enter_block ~n:(i+1) env) action)
           @ [Br(add_dummy_loc (Int32.of_int i))]),
         List.map add_dummy_edges (build_branches (i+1) rest))] in
    compiled_arg @ set_swap @
    [Block(ValBlockType (Some Types.I32Type), List.map add_dummy_edges (build_branches 1 branches))]
  else
    let compile_table labels =
      let labs_to_branches = List.mapi (fun i l -> (l, i+1)) labels in
      let default = add_dummy_loc 0l in
      (* +1 since List.init n creates cases for 0 up to (n-1) *)
      List.init (max_label + 1) (fun l -> match List.assoc_opt l labs_to_branches with
        | Some b -> add_dummy_loc (Int32.of_int b) | None -> default) in
    let rec build_branches i seen = function
      (* Base case, do actual arg eval, branch table and default case *)
      | [] ->
       [Block(ValBlockType None, (* Only left by branch table *)
        List.map add_dummy_edges
          (* enter_block not necessary for just compiling immediates *)
          ((compile_imm env arg) @
          decode_num @ [BrTable (compile_table seen, add_dummy_loc 0l)]))]
          @ (compile_block (enter_block ~n:(i + 1) env) default) @ [Br(add_dummy_loc (Int32.of_int i))]
      (* Some constructor case, wrap recursive call in this action + jump to end of switch *)
      | (l, action)::rest -> (Block(ValBlockType None,
        List.map add_dummy_edges
        ((build_branches (i+1) ((Int32.to_int l)::seen) rest))))
        :: (compile_block (enter_block ~n:(i+1) env) action) @ [Br(add_dummy_loc (Int32.of_int i))] in
    [Block(ValBlockType (Some Types.I32Type), List.map add_dummy_edges (build_branches 0 [] branches))]


and compile_block env block =
  List.flatten (List.map (compile_instr env) block)

and compile_instr env instr =
  match instr with
  | MDrop -> [Drop]
  | MImmediate(imm) -> compile_imm env imm
  | MFail j -> (match j with
      | -1l -> [Unreachable] (* trap *)
      (* get block to jump to *)
      | _ -> [Br (add_dummy_loc (List.assoc j env.handler_heights))])
  | MAllocate(alloc) -> (* New - currying appeared to not work before *)
    let new_backpatches = ref [] in
    (* Unlike for MStore, can't be mutually recursive allocations, so know closure will be swap 0 if needed *)
    let instrs = compile_allocation {env with backpatches=new_backpatches} alloc in
    let do_backpatch (_, {func_idx;variables}) =
      (* Nothing to backpatch, potentially because thing allocated wasn't a function anyway *)
      if variables = [] then [] else
      let get_swap = get_swap env 0 in
      let set_swap = set_swap env 0 in
      let tee_swap = tee_swap env 0 in
      let backpatch_var idx var =
        get_swap @ (compile_imm env var) @ [store ~offset:(Int32.of_int(4 * (idx + 2))) ();] in
      tee_swap @ get_swap @ (toggle_tag Closure) @ set_swap @ (List.flatten (List.mapi backpatch_var variables)) in
    (* Inefficient - at most one thing allocated so could use a case split rather than List map/flatten *)
    instrs @ (List.flatten (List.map do_backpatch (!new_backpatches)))

  | MDataOp(op, block) -> compile_data_op env block op
  | MUnary(op, arg) -> compile_unary env op arg
  | MBinary(op, arg1, arg2) -> compile_binary env op arg1 arg2
  | MSwitch(arg, branches, default) -> compile_switch env arg branches default
  | MStore(binds) -> compile_store env binds (* MAllocate for memory allocation, MStore for variable binding *)
  | MCallIndirect(func, args, tupled) ->
    let compiled_func = compile_imm env func in
    let ftype = add_dummy_loc (Int32.of_int (get_arity_func_type_idx env
      (if tupled then ((List.length args) + 1) else 2))) in
    let compiled_args = List.map (compile_imm env) args in
    let get_closure = get_swap env 0 in
    let tee_closure = tee_swap env 0 in
    if tupled
    then
      compiled_func @ (toggle_tag Closure) @ tee_closure @ (List.flatten compiled_args) @ get_closure
      @ [load ~offset:0l (); CallIndirect(ftype);]
    else
      List.fold_left
      (fun f compiled_arg ->
        f @ (toggle_tag Closure) @ tee_closure @ compiled_arg @
          get_closure @ [load ~offset:0l (); CallIndirect(ftype);])
      compiled_func compiled_args
  | MIf(cond, thn, els) ->
    let compiled_cond = compile_imm env cond in
    let compiled_thn = (compile_block (enter_block env) thn) in
    let compiled_els = (compile_block (enter_block env) els) in
    compiled_cond @
    [If(ValBlockType (Some Types.I32Type),
     List.map add_dummy_edges compiled_thn,
     List.map add_dummy_edges compiled_els)]

  | MWhile(cond, body) ->
    let compiled_cond = compile_block (enter_block ~n:2 env) cond in
    let compiled_body = (compile_block (enter_block ~n:2 env) body) in
    [Block(ValBlockType (Some Types.I32Type),
     List.map add_dummy_edges
        [Loop(ValBlockType (Some Types.I32Type),
         List.map add_dummy_edges
              (const_false @
              compiled_cond @
              [Test(Values.I32 Ast.IntOp.Eqz);
               BrIf (add_dummy_loc @@ Int32.of_int 1)] @
              [Drop] @
              compiled_body @
              [Br (add_dummy_loc @@ Int32.of_int 0)]))])]

  | MFor(arg, start_expr, direction, end_arg, end_expr, body) ->
    let compiled_start = compile_imm env start_expr in
    let compiled_end = compile_imm env end_expr in
    let compiled_body = (compile_block (enter_block ~n:2 env) body) in
    compiled_start @ (compile_bind ~is_get:false env arg) @
    compiled_end @ (compile_bind ~is_get:false env end_arg) @
    [Block(ValBlockType (Some Types.I32Type),
     List.map add_dummy_edges
        [Loop(ValBlockType (Some Types.I32Type),
         List.map add_dummy_edges
              (const_false @ (* Return unit value when loop fails *)
              (compile_bind ~is_get:true env arg) @
              (compile_bind ~is_get:true env end_arg) @
              [Compare(Values.I32
                (match direction with Upto -> Ast.IntOp.GtS | Downto -> Ast.IntOp.LtS))] @
              [BrIf (add_dummy_loc @@ Int32.of_int 1)] @
              compiled_body @
              (compile_bind ~is_get:true env arg) @
              [Const(encoded_const_int 1); (* For loop actually takes steps of 2 due to encoding *)
               Binary(Values.I32
                 (match direction with Upto -> Ast.IntOp.Add | Downto -> Ast.IntOp.Sub));] @
              (compile_bind ~is_get:false env arg) @
              [Br (add_dummy_loc @@ Int32.of_int 0)]))])]

  (* Creates two blocks. Inner block is usual 'try' body, outer block is that + handler body.
     If try case succeeds, Br 1 jumps to the end of the outer block so just returns result.
     Fail's within the body map to a branch to the end of the inner block, so run the handler. *)
  | MTry(i, body, handler) ->
    let body_env = enter_block ~n:2 env in
    let compiled_body = compile_block {body_env with handler_heights = (i,0l)::body_env.handler_heights} body in
    let handler_body = compile_block (enter_block env) handler in
    [Block(ValBlockType (Some Types.I32Type), (* Outer 'try/with' block, returns result *)
      List.map add_dummy_edges
       ([Block(ValBlockType None, (* inner block for body - only left by fail *)
           List.map add_dummy_edges (compiled_body @ [Br (add_dummy_loc 1l)]))]  (* try case succeeded, skip handler *)
        @ handler_body))]

  (* Not actually used? *)
  | MCallKnown(func_idx, args) ->
    let compiled_args = List.flatten (List.map (compile_imm env) args) in
    compiled_args @ [
       Call(add_dummy_loc
         (Int32.(add func_idx (of_int env.func_offset))));
    ]

let rec get_last = function
  | [] -> failwith "No last element for an empty list"
  | [x] -> x
  | x::xs -> get_last xs

let compile_function env {index; arity; stack_size; body=body_instrs} =
  let arity_int = Int32.to_int arity in
  let body_env = {env with num_args=arity_int} in
  let body = compile_block body_env body_instrs in
  let ftype_idx = get_arity_func_type_idx env arity_int in
  let ftype = add_dummy_loc Int32.(of_int ftype_idx) in
  let locals = List.append swap_slots @@ List.init (stack_size) (fun _ -> Types.I32Type) in
  let open Graph in (* so func' record type in scope *)
  {
    ftype;
    locals;
    num_swaps=List.length swap_slots;
    body = List.map add_dummy_edges body;
  }

let compute_table_size env {functions} =
  (List.length functions) + (List.length (runtime_imports ()))

let compile_imports env =
  let compile_import {mimp_name; mimp_type=(args, ret)} =
    let module_name = encode_string (Ident.name runtime_mod) in
    let item_name = encode_string (Ident.name mimp_name) in
    let idesc = let func_type = Types.FuncType(args, ret) in
        add_dummy_loc @@ Ast.FuncImport(add_dummy_loc @@ Int32.of_int @@ get_func_type_idx env func_type)
    in
    (* Wasm.Ast import' type *)
    let open Wasm.Ast in
    add_dummy_loc {
      module_name;
      item_name;
      idesc;
    } in
  let imports = List.map compile_import (runtime_imports ()) in
   (imports @
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

(* Linearise ensures that only one thing with each name gets exported, so don't need worry about duplicates *)
let compile_exports env {functions; exports; num_globals} =
  let compile_getter i {ex_name; ex_global_index} =
    let exported_name = Ident.name ex_name in
    let name = encode_string exported_name in
    let export =
      let open Wasm.Ast in
      add_dummy_loc {
        name;
        edesc=add_dummy_loc (Ast.GlobalExport (add_dummy_loc ex_global_index)); (* Export global directly *)
      } in
    export
  in
  (* Exports every function in the program, allows partially applied functions to be returned.
     Runtime functions not exported so integer used for name is offset to align with function index.
     i.e. export "i" is function i. No clashes as variable names can't start with digits *)
  let compile_lambda_export i _ =
    let name = encode_string (string_of_int (i + env.func_offset)) in
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
     compiled_lambda_exports @ (* Export each function which could be needed to execute a closure *)
        compiled_exports @ (* Export each global variable *)
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
    (List.init num_globals (fun _ ->  Types.GlobalType(Types.I32Type, Types.Mutable);)) (* No need to store the table size, externally use the exported functions, not the table *)

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


(* For debugging - if webassembly produced is invalid then throw error during compilation, and print where *)
let module_to_string compiled_module =
  (* Print module to string *)
  Wasm.Sexpr.to_string 80 @@ Wasm.Arrange.module_ compiled_module

(* Adds proper locations to each operation using Wasm module, so error can be printed out with a specific location *)
let reparse_module (module_ : Wasm.Ast.module_) =
  let open Wasm.Source in
  let as_str = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ module_) in
  let {it=script} = Wasm.Parse.string_to_module as_str in
  match script with
  | Wasm.Script.Textual(m) -> m
  | Encoded _ -> failwith "Internal error: reparse_module: Returned Encoded (should be impossible)"
  | Quoted _ -> failwith "Internal error: reparse_module: Returned Quoted (should be impossible)"

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
  List.iteri (fun idx {mimp_name;} -> Hashtbl.add imported_funcs mimp_name (Int32.of_int idx)) (runtime_imports ());
  {env with func_offset=List.length (runtime_imports ());}

let compile_wasm_module prog =
  let open Graph in
  let env = prepare init_env in
  let funcs = compile_functions env prog in
  let imports = compile_imports env in
  let exports = compile_exports env prog in
  let globals = compile_globals env prog in
  let elems = compile_elems env prog in
  let types = List.map add_dummy_loc (!(env.func_types)) in
  let ret = {
    empty_module with
    funcs;
    imports;
    exports;
    globals;
    (* Create a function table large enough to hold pointers to each function in the program *)
    tables=[add_dummy_loc {
      Ast.ttype = TableType({min=Int32.of_int(compute_table_size env prog); max=None}, FuncRefType)}];
    elems;
    types;
  } in
  (* Connect edges *)
  generate_graph ret;
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
