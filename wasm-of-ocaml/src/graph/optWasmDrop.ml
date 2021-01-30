(* Dead assigment elimination introduces 'Drop' instructions. In some places, should be able
   to propagate these backwards to remove the thing that produced them. Have to be aware of effectful
   instructions like call/set, and changes in arity like i32.binary *)
open Graph
open GraphUtils

(* So that types/functions tables don't need to be passed around. Only ever optimising one program at a time *)
let types_lst = ref ([] : Wasm.Ast.type_ list)
let func_types = ref []

let generate_type_table {types; funcs; imports} =
  func_types :=
  ((List.fold_right (fun (import : Wasm.Ast.import) lst -> match import.it.Wasm.Ast.idesc.it with
    | Wasm.Ast.FuncImport {it = i} -> (List.nth types (Int32.to_int i) )::lst
    | _ -> lst) imports []) @
  (List.map (fun func -> (List.nth types (Int32.to_int func.ftype.it))) funcs))


(* Function to find point in code where previous argument started being constructed
 Idea is that code is something like:
   [instrs to form 1st arg] @ [instrs to form 2nd arg] @ [Binary operation] @ Drop
 Might be that 2nd arg comes from a function call so can't be optimised, so we really want to handle as:
   [instrs to form 1st arg] @ [Drop] @ [instrs to form 2nd arg] @ [Drop]
 Can then separately try to optimise each of them.
 Working out where to 'insert' the drop requires walking over instructions in reverse to see where
 stack height is 1 less. Latest such point (earliest in reverse) is when 1st arg is fully constructed.
 In case where it is actually [1st arg] @ [get i; set j] @ ...
 then this is repeated to move the drop back, since it can't be the 'set j' that needs removing.
 Hence always end up at the required position.
 *)
(* Instructions iterated over and returned in reverse order (hence instructions that reduce stack increase n) *)
let rec split_stack n instrs = if n = 0 then ([], instrs)
  else if n < 0 then failwith "Stack structure error"
  else match instrs with
    | [] -> failwith "Stack structure error"
    (* Grouped by how they change the height of the stack, very similar to how propagate_drop works *)
    (* Shouldn't have instructions after anyway. May want to make this an error instead *)
    | {it=Unreachable | Return | Br _ | BrTable _}::rest -> ([], instrs)
    (* Doesn't change height of stack. Nop should already have been removed *)
    | ({it=Nop | Convert _ | Load _ | Unary _ | Test _ | LocalTee _ | MemoryGrow} as instr)::rest ->
      let above, rest = split_stack n rest in (instr::above, rest)
    (* reduce height of the stack by 1 *)
    (* Only need to consider conditional branches, should never have instructions after unconditional one.
       TODO: Check semantics make sense when conditional branches included, may want to be able to abort?
       In theory could cause a problem, but don't think this occurs in practice due to constructions used
       [1st arg] @ [tee; get (copy arg)] @ [test arg; shortcut_result; br_if] @ [2nd arg] ... *)
    | ({it=Drop | BrIf _ | Binary _ | Compare _ | LocalSet _ | GlobalSet _} as instr)::rest ->
      let above, rest = split_stack (n + 1) rest in (instr::above, rest)
    (* reduce height of stack by 2 *)
    | ({it=Select | Store _} as instr)::rest ->
      let above, rest = split_stack (n + 2) rest in (instr::above, rest)
    (* increase height of stack by 1 *)
    | ({it=LocalGet _ | GlobalGet _ | MemorySize | Const _} as instr)::rest ->
      let above, rest = split_stack (n - 1) rest in (instr::above, rest)
    (* Blocks and calls require checking type info. Need to pass the module to the function? *)
    | ({it=Block(typ, _) | Loop(typ, _) | If(typ, _, _)} as instr)::rest ->
      let new_n = match typ with
        (* actually quite heavily constrained in MVP e.g. Blocks can't take arguments *)
        | Wasm.Ast.VarBlockType {it} ->
          (match List.nth (!types_lst) (Int32.to_int it) with
           {it=Wasm.Types.FuncType (args, results)} -> n + (List.length args) - (List.length results))
        (* leaves stack unchanged *)
        | Wasm.Ast.ValBlockType None -> n
        (* puts a value on the stack *)
        | Wasm.Ast.ValBlockType (Some _) -> n - 1
      in let above, rest = split_stack new_n rest in (instr::above, rest)
    (* Doesn't actually occur - need to look up type of func (i is the func index) *)
    (* Easy to calculate since we know imports only contains functions *)
    | ({it=Call {it}} as instr)::rest ->
      let new_n = match List.nth (!func_types) ((Int32.to_int it) (* - (List.length Compilewasm.runtime_imports) *)) with
   (*   let func = List.nth (!mod_tbls).funcs ((Int32.to_int it) - (List.length Compilewasm.runtime_imports)) in
      Printf.printf "functype\n";
      let new_n = match List.nth (!mod_tbls).types (Int32.to_int func.ftype.it) with *)
        {it=Wasm.Types.FuncType (args, results)} -> n + (List.length args) - (List.length results)
      in let above, rest = split_stack new_n rest in (instr::above, rest)
    (* Need to look up type in types (i is the type index) *)
    | ({it=CallIndirect {it}} as instr)::rest ->
      let new_n = match List.nth (!types_lst) (Int32.to_int it) with
        {it=Wasm.Types.FuncType (args, results)} -> n + (List.length args) - (List.length results)
      in let above, rest = split_stack new_n rest in (instr::above, rest)

(* TODO: Work out way to test this? *)
(* reverse function body is 'drop_instr' :: 'rest' where 'rest' is 2nd argument *)
let rec propagate_drop drop_instr = function
  | [] -> [drop_instr]
  (* Can't reach drop anyway, so just remove *)
  | ({it=Unreachable|Return} as instr)::rest -> remove_instr drop_instr; instr::rest
  (* Shouldn't be generating Nops anyway *)
  | ({it=Nop} as instr)::rest ->
    remove_instr instr; propagate_drop drop_instr rest
  | ({it=Drop} as instr)::rest ->
    (* Skip past the construction of the argument to Drop. *)
    let dropped, remaining = split_stack 1 rest in
    (match remaining with
      (* Significantly more complex to reinsert drop instruction if at start of block, so just ignore *)
      | [] -> drop_instr::(propagate_drop instr rest)
      | nxt::rest -> remove_instr drop_instr;
        (propagate_drop instr dropped) @ (propagate_drop (add_after nxt Drop) remaining)
    )
  (* Drop the selecting i32, and the two values it is selecting between. (Don't think select is actually used) *)
  | ({it=Select} as instr)::rest ->
    remove_instr instr;
    let drop1 = add_after drop_instr Drop in
    let drop2 = add_after drop1 Drop in
    (* Remember that order actually reversed here *)
    propagate_drop drop2 (drop1 :: drop_instr :: rest)
  (* Avoid trying to optimise blocks. Due to branches, could end up adding in more Drop instructions than started with *)
  (* Avoid optimising branches, as previous result may actually be needed if Drop is skipped *)
  (* Can't optimise 'Call' without knowing that function is immutable (future optimisation) *)
  | ({it=LocalGet _ | GlobalGet _ | MemorySize | Const _} as instr)::rest ->
    remove_instr drop_instr; remove_instr instr; rest
  | ({it=LocalTee (x, incr, decr)} as instr)::rest ->
    remove_instr drop_instr; {instr with it=LocalSet (x, incr, decr)}::rest
  (* Assumes no out of bounds memory accesses, should be true for compiled Wasm.
     Also assumes conversions always succeed (not used anyway). *)
  | ({it=Load _ | Test _ | Unary _ | Convert _} as instr)::rest ->
    remove_instr instr; propagate_drop drop_instr rest
  | ({it=LocalSet _ | GlobalSet _} as instr)::rest ->
    (* Skip past the construction of the argument to Set *)
    let set, remaining = split_stack 1 rest in
    (match remaining with
      (* Significantly more complex to reinsert drop instruction if at start of block, so just ignore *)
      | [] -> drop_instr::instr::rest
      | nxt::rest -> remove_instr drop_instr;
        instr::set @ (propagate_drop (add_after nxt Drop) remaining)
    )
  | ({it=Compare _ | Binary _} as instr)::rest ->
    remove_instr instr;
    let drop = add_after drop_instr Drop in
    propagate_drop drop (drop_instr :: rest)
  | instrs -> drop_instr::instrs (* do nothing *)

(* code passed over in reverse direction, hence reversing block bodies before recursively searching *)
let rec remove_drops = function
  | [] -> []
  | ({it=Drop} as instr)::rest -> propagate_drop instr rest
  | ({it=Block(typ, body)} as instr)::rest ->
    {instr with it=Block(typ, List.rev (remove_drops (List.rev body)))}:: (remove_drops rest)
  | ({it=Loop(typ, body)} as instr)::rest ->
    {instr with it=Loop(typ, List.rev (remove_drops (List.rev body)))}:: (remove_drops rest)
  | ({it=If(typ, body1, body2)} as instr)::rest ->
    {instr with it=If(typ,
      List.rev (remove_drops (List.rev body1)),
      List.rev (remove_drops (List.rev body2)))}:: (remove_drops rest)
  | instr::rest -> instr::(remove_drops rest)

let optimise ({funcs; types} as module_) =
  types_lst := types;
  generate_type_table module_;
  {module_ with funcs=List.map (fun ({body} as f) ->
    {f with body=List.rev (remove_drops  (List.rev body))}) funcs}
