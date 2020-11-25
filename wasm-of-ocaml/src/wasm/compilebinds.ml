(* Map linast to wasmtree, ready to be compiled to actual wasm by compilewasm *)
(* Need to check conventions here line up with what compilewasm does
   e.g. putting values into closures/handling function args *)

open Wasmtree
open Linast
open LinastUtils

(* TODO: Work out what each is used for *)
type compile_env = {
    binds : binding Ident.tbl;
    (* Will do a second pass over exports due to needing to handle mutual recursive functions *)
    exported : int32 Ident.tbl;
    stack_idx : int;
    arity : int;
}

let initial_env = {
    binds = Ident.empty;
    exported = Ident.empty;
    stack_idx = 0;
    arity = 0;
}

(* Function index for doing lambda lifting *)
let lift_index = ref 0
let next_lift () = let v = !lift_index in incr lift_index; v

(* Track global variables *)
let global_table = ref (Ident.empty: (int32 * int32) Ident.tbl)
let global_index = ref 0
(* TODO: What is this doing? Why is there both a global_index and getter_index?? *)
let global_exports () = let tbl = !global_table in
  Ident.fold_all
  (fun ex_name (ex_global_index, ex_getter_index) acc -> {ex_name; ex_global_index; ex_getter_index}::acc) tbl []

let next_global_indexes id = (* According to Grain - some issue with 'hygiene'? *)
  (* No find_opt so using find_all and checking for the empty list *)
  match Ident.find_all (Ident.name id) (!global_table) with
    | [(_, (ret, ret_get))] -> Int32.to_int ret, Int32.to_int ret_get
    | [] -> (* TODO: Why these steps? How come globals seem to be accessed indirectly all the time? *)
      let ret = !global_index in
      let ret_get = next_lift () in
      global_table := Ident.add id ((Int32.of_int ret), (Int32.of_int ret_get)) !global_table;
      global_index := ret + 1;
      (ret, ret_get)
    | _ -> failwith "Ident put into global table more than once"

let find_id id env = Ident.find_same id env.binds
let find_global id env = Ident.find_same id env.exported


(* TODO: Work out purpose - why can't we just compile things in the usual recursive way? Need a queue? *)
type work_body =
  | Lin of linast_expr
  | Compiled of block

type work_element = {
    body : work_body;
    env : compile_env;
    arity : int;
    idx : int; (* TODO: Work out what a "lambda lifted" idx is *)
    stack_size : int;
}

let worklist = ref ([] : work_element list)

(* TODO: Use BatDeque to be able to cons in reverse.
         Why is this built in reverse order? May be possible to rewrite other way round *)
let worklist_add elt = worklist := !worklist @ [elt]
let worklist_pop () = match !worklist with
  | [] -> raise Not_found
  | x::xs -> worklist := xs; x

let compile_const (c : Asttypes.constant) =
  match c with
  | Const_int i -> MConstI32 (Int32.of_int i)
  | Const_string _ -> failwith "no strings yet" (* TODO: Implement strings *)
  | Const_float f_str -> failwith "no floats yet" (* TODO: Implement floats *)
  | Const_int32 i32 -> MConstI32 i32
  | Const_int64 i64 -> MConstI64 i64
 (* | Const_bool b -> if b then const_true else const_false  TODO: Handle bools specially at Linast level
    Could modify the OCaml compiler, but that would affect type checking so probably don't want to *)
  | Const_char c -> failwith "Characters not yet supported"
  | Const_nativeint _ -> failwith "Native ints not yet supported"

let compile_imm env (i : imm_expr) =
  match i.desc with
  | ImmConst c -> MImmConst(compile_const c)
  | ImmIdent id -> MImmBinding(find_id id env)
  | ImmMatchFail i -> MImmFail i (* TODO: Not yet implemented at lower level, requires tracking trap block distances *)

(* Line 106 in Grain *)

(* TODO: Understnad all of this (particularly later part) *)
let compile_function env args body : closure_data =
  let used_var_set = free_vars Ident.Set.empty body in
  let free_var_set = Ident.Set.diff used_var_set (Ident.Set.of_list args) in
  let free_vars = Ident.Set.elements free_var_set in
  (* ClosureBind represents variables accessed by looking up in closure environment *)
  let free_binds = Utils.fold_lefti (fun acc closure_idx var ->
      Ident.add var (MClosureBind(Int32.of_int closure_idx)) acc)
      Ident.empty free_vars in
  let closure_arg = Ident.create_local "$self" in (* TODO: Is the $ in name just to avoid clashes? *)
  (* Closure is made available in function by being an added first argument *)
  let new_args = closure_arg::args in
  let arg_binds = Utils.fold_lefti (fun acc arg_idx arg ->
      Ident.add arg (MArgBind(Int32.of_int arg_idx)) acc)
      free_binds new_args in
  let idx = next_lift() in (* TODO: What is this used for - gets put into the work_item - function body being compiled? *)
  let arity = List.length new_args in
  let stack_size = count_vars body in
  let func_env = {
    env with
    binds=arg_binds;
    stack_idx=0;
    arity=arity;
  } in
  let worklist_item = {
    body=Lin body;
    env=func_env;
    idx;
    arity;
    stack_size;
  } in
  worklist_add worklist_item;
  {
    func_idx=(Int32.of_int idx);
    arity=(Int32.of_int arity); (* Work out arity - possibly only for tuples so don't use initially? *)
    (* These variables should be in scope when the lambda is constructed. *)
    variables=(List.map (fun id -> MImmBinding(find_id id env)) free_vars);
  }

(* Compile_wrapper only used in process_imports for Wasmfunctions. Allows treating wasm function as closure?
   Not needed, as wasm_functions are just runtime calls, can map to directly. *)

(* TODO: Work out why no arity/stack_size - All referenced variables must also be globals?? *)
let next_global id =
  let ret, idx = next_global_indexes id in
  if ret <> ((!global_index) - 1) then (* Already allocated *)
    ret
  else
    let body = [
      MImmediate(MImmBinding(MGlobalBind (Int32.of_int ret)));
    ] in
    let worklist_item = {
      body=Compiled body;
      env=initial_env;
      idx;
      arity=0; (* <- this function cannot be called by the user, so no self argument is needed. TODO: Why not?? *)
      stack_size=0;
    } in
    worklist_add worklist_item;
    ret

(* BULK OF CODE *)
let rec compile_comp env (c : compound_expr) =
  match c.desc with
  | CSwitch(arg, branches, default) -> raise (NotImplemented __LOC__)
 (* TODO: Determine what level to handle switches at, see if table in Wasm is useful
   let compiled_arg = compile_imm env arg in
    MSwitch(compiled_arg,
            List.map (fun (lbl, body) ->
                (Int32.of_int lbl, compile_anf_expr env body)) branches,
            [MError(Runtime_errors.SwitchError, [compiled_arg])]) *)
  | CIf(cond, thn, els) ->
    MIf(compile_imm env cond, compile_linast env thn, compile_linast env els)
  | CWhile(cond, body) ->
    MWhile(compile_imm env cond, compile_linast env body)
  (* TODO: Translate here or at lower step? Should just be able to add the ident to the current stack?
           Easy to do due to succ/pred unary operators *)
  | CFor(id, start, finish, dir, body) -> raise (NotImplemented __LOC__)
  (* TODO: Understnad what Grain used box/unbox and tuples for here. Passing tuple as list of args to function?
  | CPrim1(Box, arg) ->
    MAllocate(MTuple [compile_imm env arg])
  | CPrim1(Unbox, arg) ->
    MTupleOp(MTupleGet(Int32.zero), compile_imm env arg) *)
  | CUnary(op, arg) ->
    MUnary(op, compile_imm env arg)
  | CBinary(op, arg1, arg2) ->
    MBinary(op, compile_imm env arg1, compile_imm env arg2)
  | CSetField (obj, idx, v) -> MDataOp(MSet(idx, compile_imm env v), compile_imm env obj)
  | CField (obj, idx) -> MDataOp(MGet(idx), compile_imm env obj)
  | CMakeBlock (tag, args) ->
      MAllocate(MData(tag, List.map (compile_imm env) args))
  (* TODO: Arrays *)
  | CArraySet(obj, idx, v) -> raise (NotImplemented __LOC__)
  | CArrayGet(obj, idx) -> raise (NotImplemented __LOC__)
  (* TODO: Strings/floats *)
  | CGetTag(obj) ->
    MDataOp(MGetTag, compile_imm env obj)
  | CMatchTry (i, body1, body2) -> raise (NotImplemented __LOC__) (* TODO: Work out how to handle at Wasm level *)

  | CFunction(args, body) -> (* TODO: Resolve mismatch of args!! Just have functions take 1 arg for now (tuples later) *)
    MAllocate(MClosure(compile_function env args body))
  (* TODO: Currying vs tuple mismatch - likely want to do many Function calls (can expand lower down) *)
  | CApp(f, args) ->
    (* TODO: Utilize MCallKnown - Since AppBuiltin never used, is CallDirect useful? Yes, for abs/min/max later *)
    MCallIndirect(compile_imm env f, List.map (compile_imm env) args)
 (* Use for min/max/abs operators later, to avoid implementing them in Linast instead and just have as runtime funcs
 | CAppBuiltin(modname, name, args) ->
    let builtin_idx = Int32.zero in
    MCallKnown(builtin_idx, List.map (compile_imm env) args) *)
  | CImm i -> MImmediate(compile_imm env i)

and compile_linast env expr = match expr.desc with
 | LSeq(hd, tl) -> (compile_comp env hd)::MDrop::(compile_linast env tl)
 | LLetRec(binds, body) ->
   let get_loc idx ((id, global, _) as bind) =
     match global with
     | Global -> (MGlobalBind(Int32.of_int (next_global id)), bind)
     | Local -> (MLocalBind(Int32.of_int (env.stack_idx + idx)), bind) in
   let binds_with_locs = List.mapi get_loc binds in
   let new_env = List.fold_left (fun acc (new_loc, (id, _, _)) -> (* - Should use global field?? *)
       (* Why does stack idx increase regardless - surely should only go up for non-globals? Just being cautious?? *)
       {acc with binds=Ident.add id new_loc acc.binds; stack_idx=acc.stack_idx + 1})
       env binds_with_locs in
       let wasm_binds = List.fold_left (fun acc (loc, (_, _, rhs)) ->
           (loc, (compile_comp new_env rhs)) :: acc)
           [] binds_with_locs in
       MStore(List.rev wasm_binds) :: (compile_linast new_env body) (* Store takes a list?? *)
 | LLet (id, global, bind, body) ->
   let location = (match global with (* As above but only 1 element *)
     | Global -> MGlobalBind(Int32.of_int (next_global id))
     | Local -> MLocalBind(Int32.of_int (env.stack_idx))) in
   (* Could split whole thing into global/local case to avoid adding empty space to stack? *)
   let new_env = {env with binds=Ident.add id location env.binds; stack_idx=env.stack_idx + 1} in
   let wasm_binds = [(location, (compile_comp new_env bind))] in
   MStore(wasm_binds) :: (compile_linast new_env body)
 | LCompound c -> [compile_comp env c]

let compile_work_element({body; env} : work_element) =
  match body with (* Piece together any bits of work remaining *)
  | Lin body ->
    compile_linast env body
  | Compiled block -> block

(* Fold left but on reference to a list, where list may be updated by function being applied *)
let fold_left_worklist f base =
  let rec help acc = match !worklist with
    | [] -> acc
    | _ -> help (f acc (worklist_pop())) in
  help base

let compile_remaining_worklist () =
  let compile_one funcs ((({idx=index; arity; stack_size}) as cur) : work_element) =
    let body = compile_work_element cur in
    {index=Int32.of_int index; arity=Int32.of_int arity; body; stack_size;}::funcs in
  List.rev (fold_left_worklist compile_one [])

(* lift_imports left out as Linast doesn't specify any imports currently. Only imports are the standard
   runtime ones which are implicit at this stage and always included.
   May change when abs/min/max implemented in runtime, but will still be very fixed i.e. just check if used or not *)
(* lift_imports creates a list of 'setups' - are these relevant/needed? May want to check what it does carefully *)

let transl_program (program : linast_expr) : wasm_program =
  let imports, setups, env = [], [], initial_env (* lift_imports initial__env anf_prog.imports *) in
  let main_body_stack_size = count_vars program in
  let main_body = setups @ (compile_linast env program) in
  let exports = global_exports() in
  let functions = compile_remaining_worklist() in
  {
    functions;
    imports;
    exports;
    main_body;
    main_body_stack_size;
    num_globals=(!global_index);
  }