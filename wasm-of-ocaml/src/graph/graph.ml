(* Wasm.Ast but replacing instr = instr' Source.phrase with instr = {it = instr'; pred/succ = instr list ref;}
   Compile to this rather than Wasm.Ast, then just write simple translation to Wasm.Ast that discards pred/succ and
   adds in dummy locations. *)
open Wasm
open Wasm.Types

type instr = {it : instr'; pred : instr list ref; succ : instr list ref}
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

(* Only ever initialise globals to 0 currently, so simplify global type. Also remove phrase *)
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
  imports : Ast.import list;
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

(* Edges should likely only be within functions for now, may change once call-known starts being used.
   Would also get issues with calling into imported runtime functions *)
let rec generate_graph module_ = failwith "Not implemented, join up all the pred/succ arcs (how to handle function calls?)"
