open Wasmtree
open Wasm
(* TODO: Use concatList to enable pre/post append efficiently *)
(* Grain uses deque package: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatDeque.html
   and similar lists: https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatList.html *)

(* Add parts as they appear needed, don't include whole 1100 line file at once *)

(* Associate a dummy location to a value - Wasm.Ast.instr = instr' Source.phrase so every part needs location *)
let add_dummy_loc (x : 'a) : 'a Source.phrase = Source.(x @@ no_region)

(* Needed as Wasm Ast represents module names (e.g. in import list) as "name" type, which is actually int list *)
let encode_string : string -> int list = Utf8.decode

(* TODO: Add something similar to func_types but tracking the distance to each handler block for fails. *)
(* try e1 with i -> e2   gets compiled as:
   `block Try: (block Handle: e1; branch to end of Try); e2` - fail compiled to a branch to end of Handle block
   Hence successful execution skips to end of try block whereas fails jump into the try block
   Both should be 0 arity blocks?
   Any fail -1 gets converted to a trap, fail i does lookup to get correct index (needs maintaining between blocks)
*)
type env = {
  (* (* Pointer to top of heap (needed until GC is implemented) *)
  heap_top: Wasm.Ast.var; -- likely GC related *)
  num_args: int;
  func_offset: int;
  global_offset: int;
(*  import_global_offset: int;
  import_func_offset: int;  -- unclear how much knowledge about imports will be needed
                               Since OCaml runtime should only contain functions, import_offest covers both above *)
  import_offset: int;  (* Should be able to set to just the number of functions in OCaml runtime *)

 (* func_types: Wasm.Types.func_type BatDeque.t ref;   Don't include need for double-ended queue currently *)
 func_types : Wasm.Types.func_type list ref;


  (* Allocated closures which need backpatching *)
  backpatches: (Wasm.Ast.instr' (* Concatlist.t  *) list * closure_data) list ref;
  imported_funcs: (int32 Ident.tbl) Ident.tbl;  (* TODO: May not be necessary if imports fixed to just the OCaml runtime parts *)
  (* imported_globals: (int32 Ident.tbl) Ident.tbl;  *)
  (* Number of blocks to jump through to reach each handler in scope.
     Possibly better choices to use than lists but never mind. *)
  handler_heights: (int32 * int32) list;
}

let enter_block ?(n=1l) ({handler_heights;} as env) =
  {env with handler_heights = List.map (fun (handler, height) -> (handler, Int32.add n height)) handler_heights}

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
(* TODO: Add runtime functions for compare, equals, etc. *)


(* global_imports should just be empty hence redundant - runtime should only have functions in it *)
let runtime_global_imports = []
let runtime_function_imports = [
  {
    mimp_mod=runtime_mod;
    mimp_name=alloc_ident;
    mimp_type=MFuncImport([I32Type], [I32Type]);
    mimp_setup=MSetupNone;
  };]
let runtime_imports = runtime_global_imports @ runtime_function_imports

let init_env () = {
  (* heap_top=add_dummy_loc (Int32.of_int (List.length runtime_global_imports)); -- Shouldn't need to be visible *)
  num_args=0;
  func_offset=0;
  global_offset=0; (* TODO: GrainRuntime has reloc_base and module_runtime_id - shouldn't need either hence 0 not 2 *)
 (* import_global_offset=0;  Should only need 1 offset, no constants
  import_func_offset=0; *)
  import_offset=0;
  func_types=ref [];
  backpatches=ref [];
  imported_funcs=Ident.empty;
  (* imported_globals=Ident.empty;  Shouldn't be any *)
  handler_heights = [];
}

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
(* NO TAGS OR INPUT/OUTPUT PROCESSING SO DON'T USE FOR NOW
  let conv_int32 = Int32.(mul (of_int 2)) in
  let conv_int64 = Int64.(mul (of_int 2)) in
*)
  begin
    match c with
    | MConstLiteral ((MConstLiteral _) as c) -> compile_const c
    | MConstI32 n -> Values.I32Value.to_value ((*conv_int32*) n)
    | MConstI64 n -> Values.I64Value.to_value ((*conv_int64*) n)
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

(* Untag should only be needed if GC present (especially untag_number) *)
(* encode after doing comparison etc. decode before an if/while statement *)
let encode_bool =  [
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.Shl);
  Ast.Const(const_false);
  Ast.Binary(Values.I32 Ast.IntOp.Or);
]

(* Currently no tags and true/false represented as 0/1, so do nothing for now *)
let decode_bool = []
(*
[
  Ast.Const(const_int32 31);
  Ast.Binary(Values.I32 Ast.IntOp.ShrU);
] *)

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
      [load ~offset:(4 * (3 + Int32.to_int i)) ()]
  | MImport(i) ->
    begin
      if not(is_get) then
        failwith "Internal error: attempted to emit instruction which would mutate an import"
    end;
    (* Adjust for runtime functions *)
    let slot = add_dummy_loc ((* env.import_offset ++  should be fixed *) i) in
    [Ast.GlobalGet(slot)]

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

let compile_imm (env : env) (i : immediate) : Wasm.Ast.instr' list =
  match i with
  | MImmConst c -> [Ast.Const(add_dummy_loc @@ compile_const c)]
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
  let swap_get = get_swap ~ty:Types.I32Type env 0 in
  let swap_set = set_swap ~ty:Types.I32Type env 0 in

  (*
  TODO: Not thinking about overflows/31-bit integers initially.
        Work out if overflow_safe needed or not for OCaml
  *)

  match op with
  | Add ->
    (* TODO: Removed overflow_safe bit - commented out in first case, removed in rest *)
    (* Removed all casting etc. for now. That and overflow checking may be needed once values tagged
       and treated as 31/63 bit ints. *)
    compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.Add);]
  | Sub ->
    compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.Sub);]
  | Mult ->
    (* Untag one of the numbers:
       ((a * 2) / 2) * (b * 2) = (a * b) * 2
    *)
    compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.Mul);]
  | Div -> (* Both div and rem are signed in OCaml *)
     compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.DivS);]
  | Mod -> (* Both div and rem are signed in OCaml *)
     compiled_arg1 @ compiled_arg2 @ [Ast.Binary(Values.I32 Ast.IntOp.RemS);]
  (* TODO: Divide and Modulo *)
  (* Can still occur due to how && and || are compiled when not applied to anything??
     i.e. when they are compiled to function abstractions, still use AND/OR rather than rewriting as an if-then-else *)
  (* Note - safe to recompile args since compile_imm's only side-effect is generating dummy locations *)
  | AND ->
    compiled_arg1 @
    swap_set @
    swap_get @
    decode_bool @ [ (* TODO: 'If' blocks introduce a label too, delay compiling arg1/2?? *)
      Ast.If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_loc (compile_imm (enter_block env) arg2), (* Recompile with updated trap handlers *)
             List.map add_dummy_loc swap_get)  (* swap_get replaced with compiled_arg1 - may cause some duplication *)
    ]
  | OR ->
    compiled_arg1 @
    swap_set @
    swap_get @
    decode_bool @ [
      Ast.If(ValBlockType (Some Types.I32Type),
             List.map add_dummy_loc swap_get,
             List.map add_dummy_loc (compile_imm (enter_block env) arg2))
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
  (* Append currently being mapped to a linast expression higher up, likewise min/max *)
  | Neq | Min | Max | Eq_phys | Neq_phys | Append -> failwith "Not yet implemented"

(** Heap allocations. *)
let round_up (num : int) (multiple : int) : int =
  multiple * (((num - 1) / multiple) + 1)

(** Rounds the given number of words to be aligned correctly - TODO: Why is alignment necessary? *)
let round_allocation_size (num_words : int) : int =
  round_up num_words 4

let heap_allocate env (num_words : int) =
  let words_to_allocate = round_allocation_size num_words in
  [Ast.Const(const_int32 (4 * words_to_allocate)); call_alloc env;]

(* Not sure check_memory needed *)
(* Not doing strings initially, so can leave out allocate_string/buf_to_ints *)


let allocate_closure env ?lambda ({func_idx; arity; variables} as closure_data) =
  let num_free_vars = List.length variables in
  let closure_size = num_free_vars + 3 in
  let get_swap = get_swap env 0 in
  let set_swap = set_swap env 0 in
  let access_lambda = Option.value ~default:(get_swap @ [
      Ast.Const(const_int32 (4 * round_allocation_size closure_size));
      Ast.Binary(Values.I32 Ast.IntOp.Sub);
    ]) lambda in
  env.backpatches := (access_lambda, closure_data)::!(env.backpatches);
  (* ToS (grows ->) : heap_ptr; num_free_vars; ptr; (reloc_base + func_idx + offset); ptr; arity  *)
  (heap_allocate env closure_size) @ set_swap @ get_swap @ [
    Ast.Const(const_int32 num_free_vars);
  ] @ get_swap @ [
 (*   Ast.GlobalGet(var_of_ext_global env runtime_mod reloc_base);     No modules so should be known at compile time *)
    Ast.Const(wrap_int32 (Int32.(add func_idx (of_int env.func_offset))));
 (*   Ast.Binary(Values.I32 Ast.IntOp.Add);  *)
  ] @ get_swap @ [
    Ast.Const(add_dummy_loc (Values.I32Value.to_value arity));
   (* take pairs off stack, putting arity, function index and number of free vars into closure object *)
   (* TODO: How is possibility of partial application handled, or is arity based on expanded out tuples? *)
    store ~offset:0 ();
    store ~offset:4 ();
    store ~offset:8 ();
  ] @ get_swap
  (*  @ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType); (* Apply the Lambda tag - may actually be needed *)
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ]  *)

let allocate_data env vtag elts =
  (* Grain compiler to-do: We don't really need to store the arity here. Could move this to module-static info *)
  (* Heap memory layout of ADT types:
     TODO: How many of these are actually needed? Certainly not module_tag
           Leave out value type tag for now - likely only needed for garbage collection
           Leave out type tag - type checking should ensure its never actually needed
    [ <value type tag>, (*<module_tag>*), <type_tag>, <variant_tag>, <arity>, elts ... ]
   *)
  let num_elts = List.length elts in
  let get_swap = get_swap env 0 in
  let set_swap = set_swap env 0 in
  let compile_elt idx elt =
    get_swap @
    (compile_imm env elt) @ [
      store ~offset:(4 * (idx + 2)) (); (* Would be +5 but not including value type/module/type tags *)
    ] in
   (* Would be num_elts + 5 except not including 3 tags *)
  (heap_allocate env (num_elts + 2)) @ set_swap @ get_swap
 (*   +@ [
    Ast.Const(const_int32 (tag_val_of_heap_tag_type ADTType));
    store ~offset:0 ();
  ] @ get_swap @ [
    Ast.GetGlobal(var_of_ext_global env runtime_mod module_runtime_id);
    store ~offset:0 ();
  ] @ get_swap @ (compile_imm env ttag) @ [
    store ~offset:4 ();
  ] @ get_swap *) (* @ (const_int32 vtag) *) @ [Ast.Const(wrap_int32 vtag);
    store ~offset:0 ();
  ] @ get_swap @ [
    Ast.Const(const_int32 num_elts);
    store ~offset:4 ();
  ] @ (List.flatten @@ List.mapi compile_elt elts) @ get_swap
   (* @ [
    Ast.Const(const_int32 @@ tag_val_of_tag_type (GenericHeapType (Some ADTType)));
    Ast.Binary(Values.I32 Ast.IntOp.Or);
  ] *)

let compile_allocation env alloc_type =
  match alloc_type with
  | MClosure(cdata) -> allocate_closure env cdata
  (* | MString(str) -> allocate_string env str *)
  | MData(tag, elts) -> allocate_data env tag elts

let compile_data_op env imm op =
  let block = compile_imm env imm in
  match op with
  | MGet(idx) ->
    let idx_int = Int32.to_int idx in
    (* Note that we're assuming the type-checker has done its
       job and this access is not out of bounds. *)
    block @ (* (untag TupleTagType) ::   -- probably not needed *) [
        load ~offset:(4 * (idx_int + 2)) (); (* +2 as blocks start with variant tag; arity; ... *)
      ]
  | MSet(idx, imm) ->
    let idx_int = Int32.to_int idx in
    block @ (* (untag TupleTagType) @ *) (compile_imm env imm) @ [
        store ~offset:(4 * (idx_int + 2)) ();
      ] @ (compile_imm env imm) (* Why do we put the value on the stack again after? *)
  | MGetTag ->
    block @ (* (untag (GenericHeapType (Some ADTType))) +@ *) [
      load ~offset:0 ();
    ]

(* TODO: How do backpatches work? What are they achieving that needs to be done in a separate stage?
         Gets done for every 'allocate_closure' so not just about recursive functions (unless being over-cautious) *)
(* What is this doing? Appears to just call f on the given env, but with backpatches reset *)
let collect_backpatches env f =
  let nested_backpatches = ref [] in
  let res = f {env with backpatches=nested_backpatches} in
  res, !nested_backpatches

(* Mutually recursive functions, once closures created, need to associate each variable for one of the other functions
   (or themself) with that closure. So go through and put a pointer to each needed closure in each of the closures. *)
let do_backpatches env backpatches =
  let do_backpatch (lam, {variables}) =
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    (* Put lam in the swap register *)
    let preamble = lam (* @ [   -- Leaving out tags for now - likely needed for Lambdas!
        Ast.Const(const_int32 @@ tag_val_of_tag_type LambdaTagType);
        Ast.Binary(Values.I32 Ast.IntOp.Xor);
      ] *) @ set_swap in
    let backpatch_var idx var = (* Store the var as the first free variable of the lambda *)
      get_swap @ (compile_imm env var) @ [store ~offset:(4 * (idx + 3)) ();] in
    preamble @ (List.flatten (List.mapi backpatch_var variables)) in
  (List.flatten (List.map do_backpatch backpatches))

(* Used to compile MStore(binds) instructions. Does backpatches (see above) *)
let rec compile_store env binds =
  let process_binds env =
    let process_bind (b, instr) acc =
      let store_bind = compile_bind ~is_get:false env b in
      let get_bind = compile_bind ~is_get:true env b in
      let compiled_instr = match instr with
        | MAllocate(MClosure(cdata)) -> (* TODO: Why special lambda for getting a closure? *)
          allocate_closure env ~lambda:get_bind cdata
        | _ -> compile_instr env instr in
      (compiled_instr @ store_bind) @ acc in
    List.fold_right process_bind binds [] in
  let instrs, backpatches = collect_backpatches env process_binds in
  instrs @ (do_backpatches env backpatches)

(* TODO: Look at compile_switch line 700, see if any significant benefit (without extra difficulty) of doing here
         vs when mapping into wasmtree. *)

and compile_block env block =
  List.flatten (List.map (compile_instr env) block)

(* THE CENTRAL FUNCTION USING ALL THE THINGS ABOVE *)
(* TODO: Go through and understand how each case works *)
and compile_instr env instr =
  match instr with
  | MDrop -> [Ast.Drop]
  | MImmediate(imm) -> compile_imm env imm
  | MAllocate(alloc) -> compile_allocation env alloc
  | MDataOp(op, block) -> compile_data_op env block op
  | MUnary(op, arg) -> compile_unary env op arg
  | MBinary(op, arg1, arg2) -> compile_binary env op arg1 arg2
  (* Decide level to do switches at *)
 (* | MSwitch(arg, branches, default) -> compile_switch env arg branches default  *)
  | MStore(binds) -> compile_store env binds
  (* TODO: Currying vs tuples - may need to generate many Ast.CallIndirects - take 1 arg at a time?? *)
  | MCallIndirect(func, args) ->
    let compiled_func = compile_imm env func in
    let compiled_args = List.flatten (List.map (compile_imm env) args) in
    let ftype = add_dummy_loc (Int32.of_int (get_arity_func_type_idx env (1 + List.length args))) in
    let get_swap = get_swap env 0 in
    let set_swap = set_swap env 0 in
    let all_args =
      compiled_func @
    (*  untag LambdaTagType @  -- Probably need, but ignoring tags for now *)
      set_swap @
      get_swap @
      compiled_args in

    all_args @
    get_swap @ [
      load ~offset:4 ();
      Ast.CallIndirect(ftype);
    ]

  | MIf(cond, thn, els) ->
    let compiled_cond = compile_imm env cond in
    let compiled_thn = List.map
        add_dummy_loc
        (compile_block (enter_block env) thn) in
    let compiled_els = List.map
        add_dummy_loc
        (compile_block (enter_block env) els) in
    compiled_cond @
    decode_bool @ [
      Ast.If(ValBlockType (Some Types.I32Type),
             compiled_thn,
             compiled_els);
    ]

  | MWhile(cond, body) ->
    let compiled_cond = compile_imm (enter_block ~n:2l env) cond in
    let compiled_body = (compile_block (enter_block ~n:2l env) body) in
    [Ast.Block(ValBlockType (Some Types.I32Type),
       List.map add_dummy_loc
        [Ast.Loop(ValBlockType (Some Types.I32Type),
              List.map add_dummy_loc
              ([Ast.Const const_false] @
              compiled_cond @
              decode_bool @
              [Ast.Test(Values.I32 Ast.IntOp.Eqz); (* TODO: Why the extra test? Checking for NOT true? *)
               Ast.BrIf (add_dummy_loc @@ Int32.of_int 1)] @
              [Ast.Drop] @
              compiled_body @
              [Ast.Br (add_dummy_loc @@ Int32.of_int 0)]))])]

  | MFor(arg, start_expr, direction, end_arg, end_expr, body) ->
    let compiled_start = compile_imm env start_expr in
    let compiled_end = compile_imm env end_expr in
    let compiled_body = (compile_block (enter_block ~n:2l env) body) in
    compiled_start @ (compile_bind ~is_get:false env arg) @
    compiled_end @ (compile_bind ~is_get:false env end_arg) @
    [Ast.Block(ValBlockType (Some Types.I32Type),
       List.map add_dummy_loc
        [Ast.Loop(ValBlockType (Some Types.I32Type),
              List.map add_dummy_loc
              ((compile_bind ~is_get:true env arg) @
              (compile_bind ~is_get:true env end_arg) @
              [Ast.Compare(Values.I32
                (match direction with Upto -> Ast.IntOp.GtS | Downto -> Ast.IntOp.LtS))] @
              decode_bool @
              [Ast.BrIf (add_dummy_loc @@ Int32.of_int 1)] @
         (*    [Ast.Drop] @ *)
              compiled_body @
              (compile_bind ~is_get:true env arg) @
              [Ast.Const(encoded_const_int32 1);
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
    let body_env = enter_block ~n:2l env in
    let compiled_body = compile_block {body_env with handler_heights = (i,0l)::body_env.handler_heights} body in
    let handler_body = compile_block (enter_block env) handler in
    [Ast.Block(ValBlockType (Some Types.I32Type), (* Outer 'try/with' block *)
       List.map add_dummy_loc
        ([Ast.Block(ValBlockType (Some Types.I32Type), (* inner block for body - What should blocktype be?*)
              List.map add_dummy_loc
              (compiled_body @ [Ast.Br (add_dummy_loc 1l)]))]  (* try case succeeded, skip handler *)
        @ handler_body))]

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
let compute_table_size env {imports; exports; functions} =
  (List.length functions) + (List.length imports) - (List.length runtime_global_imports)

(* TODO: Should be able to massively simplify this. Set of imports should be fixed, ignore any that aren't OcamlRuntime *)
(* TODO: Understand what all of ths does/is needed for *)
let compile_imports env {imports} =
  let compile_import {mimp_mod; mimp_name; mimp_type} =
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
(*  let table_size = compute_table_size env prog in *)
  let imports = List.map compile_import imports in
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
  (* NO PARTS OR RUNTIME WRITTEN IN JS, SO DON'T NEED A TABLE TO CALL THEM
      add_dummy_loc {
        Ast.module_name=encode_string (Ident.name runtime_mod);
        Ast.item_name=encode_string "tbl";
        Ast.idesc=add_dummy_loc (Ast.TableImport (Types.TableType({
            Types.min=Int32.of_int table_size;
            Types.max=None;
          }, Types.FuncRefType))); (* Actually no choice other than FuncRefType in current Wasm *)
      }; *)
    ])

(* Is there any need for naming extensions? Doesn't look like it except to avoid naming something _start etc.
   For now assume that never happens - convenience of being able to use actual names vs *)
(* Linearise ensures that only one thing with each name gets exported, so don't need worry about duplicates *)
let compile_exports env {functions; imports; exports; num_globals} =
  (* TODO: What are these indexes? *)
  (* `Getter` provides a simple way to access exported varaibles - provides a nullary function that just
     returns the corresponding global variable. (Function name matches name of variable)*)
  let compile_getter i {ex_name; ex_global_index; ex_getter_index} =
    let exported_name = (*"GRAIN$EXPORT$GET$" ^*) (Ident.name ex_name) in
    let name = encode_string exported_name in
    let fidx = (Int32.to_int ex_getter_index) + env.func_offset (* Is func_offset necessary? *) in
    let export =
      let open Wasm.Ast in
      add_dummy_loc {
        name;
        edesc=add_dummy_loc (Ast.FuncExport (add_dummy_loc @@ Int32.of_int fidx));
      } in
    export
  in
  let compile_lambda_export i _ =
    let name = encode_string ((* "GRAIN$LAM_" ^  -- also not needed? *) (string_of_int i)) in
    let edesc = add_dummy_loc (Ast.FuncExport(add_dummy_loc @@ Int32.of_int (i + env.func_offset))) in
    let open Wasm.Ast in
    add_dummy_loc { name; edesc } in
  let heap_adjust_idx = env.func_offset + (List.length functions) in
  let main_idx = heap_adjust_idx (* + 1 Likely Heap_top *) in
 (* let heap_adjust_idx = add_dummy_loc @@ Int32.of_int heap_adjust_idx in *)
  let main_idx = add_dummy_loc @@ Int32.of_int main_idx in
  let compiled_lambda_exports = List.mapi compile_lambda_export functions in
  let compiled_exports = List.mapi compile_getter exports in
     compiled_lambda_exports @
        compiled_exports @
        [
        (* -- Unclear why anything other than maybe a main function (if issues with shifting 31/32-bit ints) needed
          add_dummy_loc {
            Ast.name=encode_string "GRAIN$HEAP_ADJUST";
            Ast.edesc=add_dummy_loc (Ast.FuncExport heap_adjust_idx);
          };*)
          add_dummy_loc {
            Ast.name=encode_string "OCAML$MAIN";
            Ast.edesc=add_dummy_loc (Ast.FuncExport main_idx);
          };
          (*
          add_dummy_loc {
            Ast.name=encode_string (Ident.name table_size);
            (* We add one here because of heap top *)
            Ast.edesc=add_dummy_loc (Ast.GlobalExport (add_dummy_loc @@ Int32.of_int (num_globals + 1 + (List.length runtime_global_imports))));
          }; *)
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
        (* TODO: Can just be fixed? Believe the first global was used for heap_top *)
        add_dummy_loc (Ast.Const(const_int32 0)); (* What is stored in index 0?? Can this be fixed? *)
      ];
      init=List.init table_size (fun n -> (add_dummy_loc (Int32.of_int n)));
    };
  ]
let compile_globals env ({num_globals} as prog) =
    (* Linked to as above, what is the 1 offset, what goes in index 0? -- Probably 'Main' function *)
    (List.init ((*1 +*) num_globals) (fun _ -> add_dummy_loc {
         Ast.gtype=Types.GlobalType(Types.I32Type, Types.Mutable);
         Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 0)]);
       })) @
    [  (* TODO: Why do we add the table size as the bottom item of the list?? *)
      add_dummy_loc {
        Ast.gtype=Types.GlobalType(Types.I32Type, Types.Immutable);
        Ast.value=(add_dummy_loc [add_dummy_loc @@ Ast.Const(const_int32 (compute_table_size env prog))]);
      }
    ]

(* Heap_adjust seems to just move "heap_top" global up by first argument. Seems like heap_top should be
   hidden (not exported) in OCaml runtime, and that heap_adjust should be part of the alloc function *)

(* Currently unclear if this will be needed or not. Why is index -99?
   TODO: Look at Wasm spec to work out meaning of index *)
let compile_main env prog =
  compile_function env
    {
      index=Int32.of_int (-99);
      arity=Int32.zero;
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
  (*try Valid.check_module module_
  with Wasm.Valid.Invalid(region, msg) ->
    Printf.printf "%s\n" (module_to_string module_);
    failwith "Module validation failed. May want to implement 'reparse' to be able to find failing region of program"
*)

(* Fold left but with an added integer argument *)
let rec fold_lefti f e l =
  fst (List.fold_left (fun (e, i) x -> (f e i x, i+1)) (e, 0) l)

(* Sets up 'env' with all the imported components and adds them to 'prog', ready to start actual compilation *)
(* Should be able to simplify as all imports are just OCaml runtime *)
let prepare env ({imports} as prog) =
  let process_import ?dynamic_offset:(dynamic_offset=0) ?is_runtime_import:(is_runtime_import=false)
      (* Import module should be fixed - just OCaml runtime. Every import should be a is_runtime_import? *)
      (acc_env) idx {mimp_mod; mimp_name; mimp_type;} =
    let rt_idx = if is_runtime_import then idx + dynamic_offset else idx in
    (* TODO: Only module is the runtime, should be able to replace with adding it once and ignoring after that *)
    let register tbl =
      let tbl = begin
        try (* Ident tbl find_same has no optional/find_all_same version, so need to wrap in try/catch to test if present *)
          ignore (Ident.find_same mimp_mod tbl); tbl
        with
          Not_found -> Ident.add mimp_mod Ident.empty tbl
      end in
      Ident.add mimp_mod (Ident.add mimp_name (Int32.of_int rt_idx) (Ident.find_same mimp_mod tbl)) tbl
    in
    (* Don't expect to actually need imported_globals (OCaml runtime should only have funcs, no constants) so have
       removed the use of imported_globals - see Grain 1074 if actually needed *)
    let imported_funcs = begin
      match mimp_type with
      | MFuncImport _ ->
        (register acc_env.imported_funcs)
      | MGlobalImport _ -> failwith "No global imports expected, just functions"
    end in
    {acc_env with imported_funcs} in
  (* TODO: All of these should be fixed values based on the structure of the runtime
           runtime_imports is defined at very top of program. *)
  let import_offset = List.length runtime_imports in
 (* Should be redundant as to only have function imports, hence only 1 offset needed (check import_offset used everywhere)
  let import_func_offset = List.length runtime_function_imports in
  let import_global_offset = import_offset + (List.length imports) in  *)
  (* Shouldn't actually be any 'new_imports' *)
(*  let new_imports = runtime_imports @ imports in *)
  let new_env = Utils.fold_lefti (process_import ~is_runtime_import:true) env runtime_global_imports in
  let new_env = Utils.fold_lefti (process_import ~is_runtime_import:true) new_env runtime_function_imports in
  let new_env = Utils.fold_lefti (process_import ~dynamic_offset:import_offset) new_env imports in
  let global_offset = import_offset in (* replaced impor_global_offset - should be the same thing? *)
  let func_offset = global_offset - (List.length runtime_global_imports) in
  {
    new_env with
    import_offset;
  (*  import_func_offset; -- only importing
    import_global_offset; *)
    global_offset;
    func_offset;
  }, { (* Grain version (line 1080) seemed to recalculate various bits. All imports are from runtime so shouldn't need to *)
    prog with
    imports=runtime_imports;
    num_globals=prog.num_globals + import_offset;
  }

let compile_wasm_module ?env prog =
  let open Wasm.Ast in
  let env = match env with
    | None -> init_env ()
    | Some(e) -> e in
  let env, prog = prepare env prog in
  let funcs = compile_functions env prog in
  let imports = compile_imports env prog in
  let exports = compile_exports env prog in
  let globals = compile_globals env prog in
 (* let tables = compile_tables env prog in -- Grain version always left this empty - work out what it is/should be *)
  let elems = compile_elems env prog in
  let types = List.map add_dummy_loc (!(env.func_types)) in
  let ret = add_dummy_loc {
    empty_module with
    funcs;
    imports;
    exports;
    globals;
    (* No longer importing a table from Wasm so must declare our own - should be able to determine size
       but can probably just stay on side of caution (Grain runtime used fixed 1024).
       Doesn't actually require specifying a max?? *)
    tables=[add_dummy_loc {
      ttype = TableType({min=Int32.of_int(compute_table_size env prog); max=None}, FuncRefType)}];
    elems;
    types;
    (* TODO: Probably want this to replace 'main'? Need to decide how externally calling into OCaml will work.
             Could add an extra function to runtime to map js ints to ocaml ints once tags added (shift to 31-bits),
              then call that from js. *)
    start=None;
  } in
  validate_module ret;
  ret



(* May want to register an exception printer if I have issues debuggging - see Printexc.register_printer *)
let () =
  Printexc.register_printer (fun exc ->
      match exc with
      | WasmRunnerError(region, str, module_) ->
        let fmt_module _ m = Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ m) in
        let s = Printf.sprintf "WASM Runner Exception at %s: '%s'\n%a\n"
          (Wasm.Source.string_of_region region) str
          fmt_module module_ in
        Some(s)
      | _ -> None)