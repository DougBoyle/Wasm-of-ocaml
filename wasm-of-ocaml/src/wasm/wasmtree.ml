(* Low level IR - can translate directly to Wasm but keeps some conventions about flow control/types of variable bindings
   to make understanding translation + optimising clearer. Also leave out wrapper code like mapping bools -> int32s and back.
   Main differences are encoding of recursion, explicit closure creation/bindings, explicit binding locations rather than Idents
*)

(* Taken from Grain compiler's codegen/mashtree.ml *)
(* Value_tags likely not needed unless pattern matching going to be used. Only possible exception is
   equals function should throw error on functions?? Hence need start of closure to distinguish itself from anything else.
   Simple hack would be to store as [0, num_args, code_ptr, [args]] when all others are [len, tag, [vals]] so 0 => fun
Similarly not interacting with strings or DOM for now so can completely ignore them for now *)

(* OCaml floats are 64-bit *)
type float32 = float
type float64 = float

(* TODO: What are Sexps and why are they used (See https://github.com/janestreet/sexplib) *)
(*
let prim1_of_sexp, sexp_of_prim1 = Parsetree.prim1_of_sexp, Parsetree.sexp_of_prim1
let prim2_of_sexp, sexp_of_prim2 = Parsetree.prim2_of_sexp, Parsetree.sexp_of_prim2
*)

type unop = Linast.unop =
  | Not
  (* integer *)
  | UnNeg
  | UnAdd (* Identity - part of OCaml due to principle of least suprise, just the identity op *)
  | Succ
  | Pred
  | Abs

(* TODO: Some of these handled by giving a Linast expression for them higher up, move that to a wasm definition/function
         and hence make use of the operators here. *)
type binop = Linast.binop =
  | Eq
  | Neq
  | LT
  | GT
  | LTE
  | GTE
  | Compare
  | Min
  | Max
  | Eq_phys
  | Neq_phys
  (* boolean *)
  | AND
  | OR
  (* integer *)
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  (* list *)
  | Append

(* Types within the WASM output *)
type asmtype =
  | I32Type
  | I64Type
  | F32Type
  | F64Type

type constant =
  | MConstI32 of int32
  | MConstI64 of int64
  | MConstF32 of float
  | MConstF64 of float
  | MConstLiteral of constant (* Special case for things which should not be encoded -- encoding may not be necessary anyway *)


type binding =
  (* Args (function arguments) and Local (free variables) and swap (register memory) all correspond to local memory *)
  | MArgBind of int32
  | MLocalBind of int32 (* local stack of function *)
  | MGlobalBind of int32 (* global variables *)
  (* Value held in closure which is the 0th local. Use that as a base for memory indirect access *)
  | MClosureBind of int32
  (* SwapBinds use local memory for a couple registers, due to inflexibility of a stack machine *)
  | MSwapBind of int32 (* Used like a register would be *)
  | MImport of int32 (* Index into list of imports -- not needed? All handled by being slightly higher level than wasm for now? *)

type immediate =
  | MImmConst of constant
  | MImmBinding of binding
  | MFail of int32 (* Match failure - compiles to a branch out to the block for the switch of same label *)

type closure_data = {
  func_idx: int32;
  arity: int32;
  variables: immediate list; (* How are these used? *)
} 

type allocation_type =
  | MClosure of closure_data
  | MData of immediate * immediate list (* Generalise tuples/adts/records etc. all into same type. Closures are separate *)
  (* | MTuple of immediate list
  | MADT of immediate * immediate * immediate list (* Type Tag, Variant Tag, Elements *)  *)
  | MString of string


type tag_op =
  | MCheckTag
  | MAssertTag
  | MAddTag
  | MRemoveTag

(* TODO: Purpose? *)
type arity_operand =
  | MLambdaArity
  | MDataArity


type arity_op =
  | MGetArity
  | MAssertArity of int32

type data_op =
  | MGet of int32
  | MSet of int32 * immediate

type instr =
  | MImmediate of immediate
  | MCallKnown of int32 * immediate list (* Optimized path for statically-known function names *)
  | MCallIndirect of immediate * immediate list
 (* | MError of grain_error * immediate list    No exceptions *)
  | MAllocate of allocation_type
  | MTagOp of tag_op * (* tag_type * *) immediate
  | MArityOp of arity_operand * arity_op * immediate
  | MIf of immediate * block * block
  | MWhile of block * block
  (* TODO: Should my IR compile down switches to ifs at this point? *)
  | MSwitch of immediate * (int32 * block) list * block (* value, branches, default *)
  | MUnary of unop * immediate
  | MBinary of binop * immediate * immediate
  | MDataOp of data_op * immediate
  | MStore of (binding * instr) list (* Items in the same list have their backpatching delayed until the end of that list *)
  | MDrop (* Ignore the result of the last expression. Used for sequences. *)

and block = instr list 

(* No modules so how much are these needed (for now, may extend later) *)
type import_type =
  | MFuncImport of asmtype list * asmtype list
  | MGlobalImport of asmtype


type import_kind =
  | MImportWasm
  | MImportGrain


type import_setup =
  | MCallGetter
  | MWrap of int32
  | MSetupNone

(* Again, imports fixed and exports limited, are these actually needed? *)
type import = {
  mimp_mod: Ident.t;
  mimp_name: Ident.t;
  mimp_type: import_type;
  mimp_kind: import_kind;
  mimp_setup: import_setup;
} 

type export = {
  ex_name: Ident.t;
  ex_global_index: int32;
  ex_getter_index: int32;
} 

type mash_function = {
  index: int32;
  arity: int32; (* Note from grain code to do: Proper typing of arguments *)
  body: block;
  stack_size: int;
} 

type mash_program = {
  functions: mash_function list;
  imports: import list;
  exports: export list;
  main_body: block;
  main_body_stack_size: int;
  num_globals: int;
  signature: Cmi_format.cmi_infos;
} 

(* Why these rather than just 0/1? Likely due to Grain GC needing to represent things in a certain way, can simplify for mine *)
(* TODO: Decide what actual values should be *)
let const_true =  MConstLiteral (MConstI32 (Int32.of_int 0xFFFFFFFF))
let const_false = MConstLiteral (MConstI32 (Int32.of_int 0x7FFFFFFF))
