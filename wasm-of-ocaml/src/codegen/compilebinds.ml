(* Map linast to wasmtree, ready to be compiled to actual wasm by compilewasm *)
(* Need to check conventions here line up with what compilewasm does
   e.g. putting values into closures/handling function args *)

open Bindstree
open Linast
open LinastUtils

type compile_env = {
    binds : binding Ident.tbl;
    stack_idx : int;
}

let initial_env = {
    binds = Ident.empty;
    stack_idx = 0;
}

(* Function index for doing lambda lifting *)
let lift_index = ref 0
(* Next lift is effectively the number of functions allocated,
   to keep size of function table matching indices used, must add a function to worklist each time this is called *)
let next_lift () = let v = !lift_index in incr lift_index; v

(* Track global variables *)
let global_table = ref (Ident.empty : int32 Ident.tbl)
(* Exports - distinct from globals. Not looked up so can be a list rather than table *)
let exports_list = ref ([] : export list)

let global_index = ref 0

(* Ident.find_same doesn't have an optional version *)
let find_same_opt id tbl =
  match Ident.find_same id tbl with
    | x -> Some x
    | exception Not_found -> None

let next_global id =
  match find_same_opt id (!global_table) with
    | Some ret -> ret
    | None ->
      let ret = Int32.of_int (!global_index) in
      global_table := Ident.add id ret !global_table;
      incr global_index;
      ret

(* Allocates a global and marks it as exported in exports_list *)
let next_export id =
  match find_same_opt id (!global_table) with
      | Some ret -> ret
      | None ->
      let ret = Int32.of_int (!global_index) in
      global_table := Ident.add id ret !global_table;
      exports_list := {ex_name=id; ex_global_index=ret}::(!exports_list);
      incr global_index;
      ret

let find_id id env =
  try
    Ident.find_same id env.binds
  with Not_found ->
    Format.fprintf Format.std_formatter "Compilebinds could not find binding for %a\n" Ident.print id;

    raise Not_found

type work_body =
  | Lin of linast_expr
  | Compiled of block

type work_element = {
    body : work_body;
    env : compile_env;
    arity : int;
    idx : int;
    stack_size : int;
}

let worklist = ref ([] : work_element list)

(* TODO: Use BatDeque to be able to cons in reverse.
         Why is this built in reverse order? May be possible to rewrite other way round *)
let worklist_add elt = worklist := !worklist @ [elt]
let worklist_pop () = match !worklist with
  | [] -> failwith "Worklist empty"
  | x::xs -> worklist := xs; x

let compile_const (c : Asttypes.constant) =
  match c with
  | Const_int i -> MConstI32 (Int32.of_int i)
  | Const_string _ -> failwith "no strings yet" (* TODO: Implement strings *)
  | Const_float f_str -> MConstF64 (float_of_string f_str)
  | Const_int32 i32 -> MConstI32 i32
  | Const_int64 i64 -> MConstI64 i64 (* TODO: Should this be supported currently? Likely fails in practice *)
  | Const_char c -> failwith "Characters not yet supported"
  | Const_nativeint _ -> failwith "Native ints not yet supported"

let compile_imm env i =
  match i.i_desc with
  | ImmConst c -> MImmConst(compile_const c)
  | ImmIdent id -> MImmBinding(find_id id env)

let compile_function env args body tupled : closure_data =
  (* Safe to keep any globals in the environment for compiling the function body *)
  let global_vars, global_binds = Ident.fold_all (fun id bind (vars, tbl) -> match bind with
     |  MGlobalBind _ -> id::vars, Ident.add id bind tbl
     | _ -> vars, tbl
    ) env.binds ([], Ident.empty) in

  let free_var_set = free_vars (Ident.Set.of_list (global_vars @ args)) body in
  let args, rest = if tupled then args, []
    else match args with x::xs -> [x], xs | _ -> failwith "Function of no args" in

  let free_vars = Ident.Set.elements free_var_set in
 (* ClosureBind represents variables accessed by looking up in closure environment.
    Set is built off of global_binds, a table of the preserved bindings *)
  let free_binds = Utils.fold_lefti (fun acc closure_idx var ->
      Ident.add var (MClosureBind(Int32.of_int closure_idx)) acc)
      global_binds free_vars in
  let closure_arg = Ident.create_local "$self" in
  (* Closure is made available in function by being an added first argument *)
  (* Unroll currying here, so only ever 2 args *)
  let new_args = closure_arg::args in
  let arg_binds = Utils.fold_lefti (fun acc arg_idx arg ->
      Ident.add arg (MArgBind(Int32.of_int arg_idx)) acc)
      free_binds new_args in
  let idx = next_lift() in (* function index to access function in WebAssembly *)
  (* Number of vars declared in body hence number of locals added.
     ASSERTION: At least as large as final stack_idx when compiling.
                If larger, just get unused slots. If too small, get out of bounds local accesses. *)
  let stack_size = count_vars body in
  let func_env = {
    binds=arg_binds;
    stack_idx=0;
  } in
  let worklist_item = {
    body=Lin (match rest with [] -> body | _ -> LinastExpr.compound (Compound.mkfun rest body));
    env=func_env;
    arity=List.length new_args;
    idx;
    stack_size;
  } in
  worklist_add worklist_item;
  {
    func_idx=(Int32.of_int idx);
    arity=Int32.of_int (List.length new_args);
    (* These variables should be in scope when the lambda is constructed. *)
    variables=(List.map (fun id -> MImmBinding(find_id id env)) free_vars);
  }

(* Store which Idents are to functions to be called as tupled rather than curried.
   Only marked as such if already verified that all calls must use that ident (and calls rewritten to fully apply) *)
let tupled_functions = ref Ident.Set.empty

(* BULK OF CODE *)
let rec compile_comp env c =
  match c.c_desc with
  (* Switches left till bottom level to potentially make use of Br_Table *)
  | CSwitch(arg, branches, default) -> let compiled_arg = compile_imm env arg in
    MSwitch(compiled_arg,
       List.map (fun (lbl, body) -> (Int32.of_int lbl, compile_linast false env body)) branches,
       match default with Some e -> compile_linast false env e | None -> [MFail (-1l)])
  | CIf(cond, thn, els) ->
    MIf(compile_imm env cond, compile_linast false env thn, compile_linast false env els)
  | CWhile(cond, body) ->
    MWhile(compile_linast false env cond, compile_linast false env body)
  | CFor(id, start, finish, dir, body) ->
    let start_bind = MLocalBind(Int32.of_int (env.stack_idx)) in
    let end_bind = MLocalBind(Int32.of_int (env.stack_idx + 1)) in (* Don't re-evaluate limits of loop *)
    let new_env = {binds=Ident.add id start_bind env.binds; stack_idx=env.stack_idx + 2} in
    MFor(start_bind, compile_imm env start, dir, end_bind, compile_imm env finish, compile_linast false new_env body)
  | CUnary(op, arg) ->
    MUnary(op, compile_imm env arg)
  | CBinary(op, arg1, arg2) ->
    MBinary(op, compile_imm env arg1, compile_imm env arg2)
  | CSetField (obj, idx, v) -> MDataOp(MSet(Int32.of_int idx, compile_imm env v), compile_imm env obj)
  | CField (obj, idx) -> MDataOp(MGet(Int32.of_int idx), compile_imm env obj)
  | CMakeBlock (tag, args) ->
      MAllocate(MData(Int32.of_int tag, List.map (compile_imm env) args))
  | CArraySet(obj, idx, v) ->
    MDataOp(MArraySet(compile_imm env idx, compile_imm env v), compile_imm env obj)
  | CArrayGet(obj, idx) -> MDataOp(MArrayGet(compile_imm env idx), compile_imm env obj)
  (* TODO: Strings *)
  | CGetTag(obj) ->
    MDataOp(MGetTag, compile_imm env obj)
  | CMatchTry (i, body1, body2) ->
    MTry(i, compile_linast false env body1, compile_linast false env body2)
  | CFunction(args, body) when List.mem Tupled (!(c.c_annotations)) ->
    MAllocate(MClosure(compile_function env args body true))
  | CFunction(args, body) ->
    MAllocate(MClosure(compile_function env args body false))
  | CApp(({i_desc=ImmIdent id} as f), args) when Ident.Set.mem id (!(tupled_functions)) ->
    MCallIndirect(compile_imm env f, List.map (compile_imm env) args, true)
  | CApp(f, args) ->
    (* TODO: Utilize MCallKnown - Since AppBuiltin never used, is CallDirect useful? Maybe for optimisation when target known *)
    MCallIndirect(compile_imm env f, List.map (compile_imm env) args, false)
  | CMatchFail i -> MFail i
  | CImm i -> MImmediate(compile_imm env i)
  (* Updates the ident, guarenteed to be a local varaible in the function so assignment allowed *)
  (* Done this way, Assign doesn't have a result, so places that insert an assign must explicitly put 0 after *)
  | CAssign (id, imm) -> MDataOp(MAssign (find_id id env), compile_imm env imm)

(* toplevel boolean indicates that a variable should be stored as a global rather than a local.
   Recursive calls for function bodies or within compound terms all set toplevel to false
   Global only expected at toplevel, indicates that variable is exported (should change name). *)
and compile_linast toplevel env expr =
 match expr.desc with
 | LSeq(hd, tl) -> (compile_comp env hd)::MDrop::(compile_linast toplevel env tl)
 | LLetRec(binds, body) ->
   let get_loc idx ((id, global, compound) as bind) =
     if List.mem Tupled (!(compound.c_annotations)) then
       tupled_functions := Ident.Set.add id (!tupled_functions);
     (if toplevel then
       match global with
       | Export -> MGlobalBind(next_export id)
       | Local | Mut -> MGlobalBind(next_global id)
     else MLocalBind(Int32.of_int (env.stack_idx + idx))), bind in (* Should never see 'Global' if not toplevel *)
   let binds_with_locs = List.mapi get_loc binds in
   let new_env = List.fold_left (fun acc (new_loc, (id, _, _)) -> (* - Should use global field?? *)
       (* Cautious/simple approach - may end up using more stack variables than necessary.
          This is unrelated to free vars. Determines the number of local slots to include in a function *)
       {binds=Ident.add id new_loc acc.binds; stack_idx=acc.stack_idx + 1})
       env binds_with_locs in
       let wasm_binds = List.fold_left (fun acc (loc, (_, _, rhs)) ->
           (loc, (compile_comp new_env rhs)) :: acc)
           [] binds_with_locs in
       MStore(List.rev wasm_binds) :: (compile_linast toplevel new_env body) (* Store takes a list?? *)
 | LLet (id, global, bind, body) ->
   if List.mem Tupled (!(bind.c_annotations)) then
     tupled_functions := Ident.Set.add id (!tupled_functions);
   let location = if toplevel then (match global with (* As above but only 1 element *)
     | Export -> MGlobalBind(next_export id)
     | Local | Mut -> MGlobalBind(next_global id))
     else MLocalBind(Int32.of_int (env.stack_idx)) in (* Should never see 'Global' if not toplevel *)
   (* only need another stack variable if the thing bound to wasn't a global. Reduces local vars in main *)
   let new_env = {binds=Ident.add id location env.binds; stack_idx=env.stack_idx +
    match global with Local | Mut -> 1 | Export -> 0} in
   let wasm_binds = [(location, (compile_comp env bind))] in
   MStore(wasm_binds) :: (compile_linast toplevel new_env body)
 | LCompound c -> [compile_comp env c]

let compile_work_element({body; env} : work_element) =
  match body with (* Piece together any bits of work remaining *)
  | Lin body ->
    compile_linast false env body
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

let transl_program (program : linast_expr) : binds_program =
  let env = initial_env in
  let main_body_stack_size = count_vars program in
  let main_body = compile_linast true env program in
  let exports = !exports_list in
  let functions = compile_remaining_worklist() in
  {
    functions;
    exports;
    main_body;
    main_body_stack_size;
    num_globals=(!global_index);
  }