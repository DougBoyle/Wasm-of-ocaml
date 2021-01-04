(* Wasm.Ast but replacing instr = instr' Source.phrase with instr = {it = instr'; pred/succ = instr list ref;}
   Compile to this rather than Wasm.Ast, then just write simple translation to Wasm.Ast that discards pred/succ and
   adds in dummy locations. *)
open Wasm
open Wasm.Types

module Set32 = Set.Make(Int32)

let id = ref 0
let get_id () = incr id; !id

(* Currently, 'call' instructions just point to the following instruction rather than between other functions/imports.
   Do to possible branching at 'if' blocks, can have an unlimited number of possible successors *)
(* So far, just use to detect unused locals. Will be more useful when CSE + constant propagation added *)
(* TODO: Could probably do with an 'iter/map' function to traverse blocks efficiently? *) (* id uniquely identifies node *)
type instr = {it : instr'; pred : instr list ref; succ : instr list ref; live : Set32.t ref; id : int}
and instr' =
  | Unreachable
  | Nop
  | Drop
  | Select
  | Block of Ast.block_type * instr list
  | Loop of Ast.block_type * instr list
  | If of Ast.block_type * instr list * instr list
  | Br of Ast.var
  | BrIf of Ast.var
  | BrTable of Ast.var list * Ast.var
  | Return
  | Call of Ast.var
  | CallIndirect of Ast.var
  | LocalGet of Ast.var
  | LocalSet of Ast.var
  | LocalTee of Ast.var
  | GlobalGet of Ast.var
  | GlobalSet of Ast.var
  | Load of Ast.loadop
  | Store of Ast.storeop
  | MemorySize
  | MemoryGrow
  | Const of Ast.literal
  | Test of Ast.testop
  | Compare of Ast.relop
  | Unary of Ast.unop
  | Binary of Ast.binop
  | Convert of Ast.cvtop

(* Only ever initialise globals to 0 currently, so simplify global type. Also remove phrase.
   All globals are actually the exact same type, could just have an int for the number of globals. *)
type global = global_type
type func =
{
  ftype : Ast.var;
  locals : value_type list;
  body : instr list;
}

type module_ = (* TODO: Remove from this representation all the ones I don't use *)
{
  types : Ast.type_ list;
  globals : global list;
  tables : Ast.table list;
  memories : Ast.memory list;
  funcs : func list;
  elems : Ast.var list Ast.segment list;
  data : string Ast.segment list;
  imports : Ast.import list; (* TODO: Can simplify this, always import the same functions + memory *)
  exports : Ast.export list;
}

let empty_module =
{
  types = [];
  globals = [];
  tables = [];
  memories = [];
  funcs = [];
  elems  = [];
  data = [];
  imports = [];
  exports = [];
}

let add_dummy_loc (x : 'a) : 'a Source.phrase = Source.(x @@ no_region)

let translate_globals globals =
  List.map (fun global -> add_dummy_loc {Ast.gtype=global;
    Ast.value=add_dummy_loc [add_dummy_loc (Ast.Const (add_dummy_loc (Values.I32Value.to_value 0l)))]}) globals

let rec translate_instr instr = add_dummy_loc (match instr.it with
  | Unreachable -> Ast.Unreachable
  | Nop -> Ast.Nop
  | Drop -> Ast.Drop
  | Select -> Ast.Select
  | Block (blocktype, body) -> Ast.Block(blocktype, List.map translate_instr body)
  | Loop (blocktype, body) -> Ast.Loop(blocktype, List.map translate_instr body)
  | If (blocktype, body1, body2) -> Ast.If(blocktype, List.map translate_instr body1, List.map translate_instr body2)
  | Br i -> Ast.Br i
  | BrIf i -> Ast.BrIf i
  | BrTable (vars, i) -> Ast.BrTable (vars, i)
  | Return -> Ast.Return
  | Call i -> Ast.Call i
  | CallIndirect i -> Ast.CallIndirect i
  | LocalGet i -> Ast.LocalGet i
  | LocalSet i -> Ast.LocalSet i
  | LocalTee i -> Ast.LocalTee i
  | GlobalGet i -> Ast.GlobalGet i
  | GlobalSet i -> Ast.GlobalSet i
  | Load op -> Ast.Load op
  | Store op -> Ast.Store op
  | MemorySize -> Ast.MemorySize
  | MemoryGrow -> Ast.MemoryGrow
  | Const c -> Ast.Const c
  | Test op -> Ast.Test op
  | Compare op -> Ast.Compare op
  | Unary op -> Ast.Unary op
  | Binary op -> Ast.Binary op
  | Convert op -> Ast.Convert op)

let translate_funcs funcs =
  List.map (fun {ftype; locals; body} -> add_dummy_loc {ftype; locals; Ast.body=List.map translate_instr body}) funcs

let translate_to_wasm {types; globals; tables; memories; funcs; elems; data; imports; exports} =
  add_dummy_loc {
  types;
  globals = translate_globals globals;
  tables; memories;
  funcs = translate_funcs funcs;
  Ast.start=None;
  elems; data; imports; exports;
  }

(* unwraps any blocks to find the first actual instruction (including branches)
   return a list since an 'if' has two starts, depending on condition outcome. *)
let rec get_start = function
  | [] -> []
  | {it=(Block(_, body)|Loop(_,body))}::rest ->
    (match get_start body with [] -> get_start rest | starts -> starts)
  | {it=If(_, body1, body2)}::rest ->
    (match get_start body1, get_start body2 with
     | [], starts -> (get_start rest) @ starts
     | starts, [] -> starts @ (get_start rest)
     | starts1, starts2 -> starts1 @ starts2)
  | instr::_ -> [instr]

(* Gets the start of function. When function called,  nothing on stack so can't start with If, hence only 1 instruction *)
let func_start {body} = match get_start body with
  | [instr] -> instr | _ -> failwith "Function with no single start instruction"

let enter_block labels = List.map (fun (i, starts) -> (Int32.add i 1l, starts)) labels

let link_to instr goes_to =
  instr.succ := goes_to @ (!(instr.succ));
  List.iter (fun {pred} -> pred := instr::(!pred)) goes_to

(* add pred/succ labels to instructions. (Based on execution sequences, not value dependencies (e.g. unary/binary))
   'Block' instructions shouldn't be linked in to anything? Will need a function to traverse through blocks to get start *)
(* on_exit is successors of the current block i.e. instructions to link to if end of block reached.
   Can't get from Labels as jump != fall through for Loop blocks *)
(* TODO: Should 'unreachable' be treated specially? Seeing as it can never actually be reached, should unlink any edges *)
let rec process_instrs on_exit labels = function
  (* TODO: Probably need a 1-element list case? *)
  (* Should this case ever actually occur? Maybe in empty function? In which case it shouldn't have been generated *)
  | [] -> failwith "Empty instructions"
  (* Blocks *)
  | {it=Block(_, body)}::rest -> (match get_start rest with
    (* end of this block, so branches out of this block go to whatever is in on_exit *)
    | [] -> process_instrs on_exit ((0l, on_exit)::(enter_block labels)) body
    | instrs -> process_instrs instrs ((0l, instrs)::(enter_block labels)) body;
      process_instrs on_exit labels rest
  )
  | {it=Loop(_, body)}::rest -> (match get_start rest with
    (* end of this block, so branches out of this block go to whatever is in on_exit *)
    | [] -> process_instrs on_exit ((0l, get_start body)::(enter_block labels)) body
    | instrs -> process_instrs instrs ((0l, get_start body)::(enter_block labels)) body;
      process_instrs on_exit labels rest
  )
  | {it=If(_, body1, body2)}::rest -> (match get_start rest with
    (* end of this block, so branches out of this block go to whatever is in on_exit *)
    | [] -> process_instrs on_exit ((0l, on_exit)::(enter_block labels)) body1;
            process_instrs on_exit ((0l, on_exit)::(enter_block labels)) body2
    | instrs -> process_instrs instrs ((0l, instrs)::(enter_block labels)) body1;
                process_instrs instrs ((0l, instrs)::(enter_block labels)) body2;
                process_instrs on_exit labels rest
  )
  (* Branches *)
  | ({it=Br {it=i}} as instr)::_ -> (* Should always find label in labels *)
    let goes_to = List.assoc i labels in
    link_to instr goes_to
  | ({it=BrIf {it=i}} as instr)::rest -> let goes_to = List.assoc i labels in
      link_to instr goes_to;
      (match get_start rest with
        | [] -> link_to instr on_exit
        | instrs -> link_to instr instrs; process_instrs on_exit labels rest
      )
  (* Link to all possible branch targets *)
  | (({it=BrTable (vars, var)}) as instr)::_ ->
      List.iter (fun ({it=i} : Ast.var) -> let goes_to = List.assoc i labels in link_to instr goes_to) (var::vars)
   (* Return completely escapes the function, so has no successor *)
  | {it=Return}::_ -> ()
  (* Avoid matching [instr] in case that rest is just Block([]), although this should never happen *)
  | instr::rest -> (match get_start rest with
    | [] -> link_to instr on_exit
    | instrs -> link_to instr instrs; process_instrs on_exit labels rest)

(* Whenever an optimisation happens which can remove/insert nodes, either need to patch up or regenerate graph *)
(* TODO: Add regenerate function and functions to add/remove from succ/pred lists (keeping unique membership) *)
let rec generate_graph module_ =
  List.iter (fun {body} -> process_instrs [] [] body) module_.funcs

(* For searching/updating predecessor/successor lists. Optimisations can modify both the actual operation
   and the successors/predecessors of a node (e.g. if an instruction is removed) so need a field for comparisons only. *)
let rec instr_eq {id=id1} {id=id2} = id1 = id2
