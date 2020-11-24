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
(* TODO: When is reset actually needed? *)
let reset_lift () = lift_index := 0
let next_lift () = let v = !lift_index in incr lift_index; v

(* Track global variables *)
let global_table = ref (Ident.empty: (int32 * int32) Ident.tbl)
let global_index = ref 0
(* TODO: What is this doing? Why is there both a global_index and getter_index?? *)
let global_exports = let tbl = !global_table in
  Ident.fold_all
  (fun ex_name (ex_global_index, ex_getter_index) acc -> {ex_name; ex_global_index; ex_getter_index}::acc) tbl []
(* Is reset needed? *)
let reset_globals () = global_table := Ident.empty; global_index := 0

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

let worklist_reset () = worklist := []
(* TODO: Use BatDeque to be able to cons in reverse.
         Why is this built in reverse order? May be possible to rewrite other way round *)
let worklist_add elt = worklist := !worklist @ [elt]
let worklist_empty () = !worklist = []
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
  let used_var_set = free_vars body in
  let free_var_set = Ident.Set.diff used_var_set @@ Ident.Set.of_list args in
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

(* TODO: What is this doing? Where are wrappers needed/used? Just consists of a function call. *)
let compile_wrapper env real_idx arity : closure_data =
  let body = [
    MCallKnown(Int32.of_int real_idx, List.init arity (fun i -> MImmBinding(MArgBind(Int32.of_int (i + 1)))));
  ] in
  let idx = next_lift() in
  let lam_env = {
    env with
    binds=Ident.empty;
    stack_idx=0;
    arity=arity + 1; (* Add own closure as an argument -- can optimise in future if no free vars *)
  } in
  let worklist_item = {
    body=Compiled body;
    env=lam_env;
    idx;
    arity=arity + 1;
    stack_size=0;
  } in
  worklist_add worklist_item;
  {
    func_idx=(Int32.of_int idx);
    arity=(Int32.of_int (arity + 1));
    variables=[];
  }

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
  | CSwitch(arg, branches, default) -> raise NotImplemented
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
  | CFor(id, start, finish, dir, body) -> raise NotImplemented
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
  | CArraySet(obj, idx, v) -> raise NotImplemented
  | CArrayGet(obj, idx) -> raise NotImplemented
  (* TODO: Strings/floats *)
  | CGetTag(obj) ->
    MDataOp(MGetTag, compile_imm env obj)
  | CMatchTry (i, body1, body2) -> raise NotImplemented (* TODO: Work out how to handle at Wasm level *)
  | CFunction(args, body) -> (* TODO: Resolve mismatch of args!! Just have functions take 1 arg for now (tuples later) *)
    MAllocate(MClosure(compile_function env args body))
  | CApp(f, args) ->
    (* TODO: Utilize MCallKnown - Since AppBuiltin never used, is CallDirect useful? Yes, for abs/min/max later *)
    MCallIndirect(compile_imm env f, List.map (compile_imm env) args)
 (* Use for min/max/abs operators later, to avoid implementing them in Linast instead and just have as runtime funcs
 | CAppBuiltin(modname, name, args) ->
    let builtin_idx = Int32.zero in
    MCallKnown(builtin_idx, List.map (compile_imm env) args) *)
  | CImm i -> MImmediate(compile_imm env i)

and compile_linast env a = failwith ""