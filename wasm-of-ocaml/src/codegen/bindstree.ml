(* Low level IR - Replaces Ident identifiers with integer indexes into Local/Arg/Closure/Global,
                  makes memory allocations explicit, including storing tag,
                  moves all functions to top-level, doing backpatching when closure allocated *)

(* Copied out to make visible without opening Linast in compilewasm *)
type unop = Linast.unop =
  | Not
  | UnNeg
  | UnAdd
  | Succ
  | Pred
  | Abs
  | FUnNeg
  | FSqrt

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
  | AND
  | OR
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Append
  | FAdd
  | FSub
  | FMult
  | FDiv

(* Types within the WASM output *)
type asmtype = Wasm.Types.value_type =
  | I32Type
  | I64Type
  | F32Type (* Should never actually occur, but want to use same type as Wasm.Types.value_type *)
  | F64Type

type constant =
  | MConstI32 of int32
  | MConstI64 of int64 (* 64-bit ints not yet supported *)
  | MConstF64 of float

type binding =
  (* Args (function arguments) and Local (free variables) and swap (register memory) all correspond to local memory *)
  | MArgBind of int32
  | MLocalBind of int32 (* local stack of function *)
  | MGlobalBind of int32 (* global variables *)
  (* Value held in closure which is the 0th local. Use that as a base for memory indirect access *)
  | MClosureBind of int32
  (* SwapBinds use local memory for a couple registers, due to inflexibility of a stack machine *)
  | MSwapBind of int32 (* Used like a register would be *)

type immediate =
  | MImmConst of constant
  | MImmBinding of binding

type closure_data = {
  func_idx: int32;
  arity: int32;
  variables: immediate list; (* How are these used? *)
} 

type allocation_type =
  | MClosure of closure_data
  | MData of int32 * immediate list
 (* | MString of string -- Leave strings for now *)

(* Arrays represented identically but with tag being the length *)
type data_op =
  | MGet of int32
  | MSet of int32 * immediate
  | MGetTag
  | MArrayGet of immediate
  | MArraySet of immediate * immediate
  (* Allows mutable locals rather than using memory for tail call optimisation *)
  | MAssign of binding (* binding is the local to update *)

type instr =
  | MImmediate of immediate
  | MFail of int32 (* Match failure - compiles to a branch out to the block for the switch of same label *)
  | MCallKnown of int32 * immediate list (* Statically-known function names *)
  (* bool indicates arguments are passed as tuple rather than curried *)
  | MCallIndirect of immediate * immediate list * bool
  (* memory allocation *)
  | MAllocate of allocation_type
  | MIf of immediate * block * block
  | MWhile of block * block
  (* Ident no longer needed - should be encoded as (2?) local variables to get/set? *)
  (*   for      x   =  e1              to             (y = )  e2     do    e3  *)
  (* After being evaluated at start, changing value doesn't change limit of loop, so need to save to var *)
  | MFor of binding * immediate * Asttypes.direction_flag * binding * immediate * block
  | MTry of int32 * block * block
  | MSwitch of immediate * (int32 * block) list * block (* TODO: Default case could be made optional *)
  | MUnary of unop * immediate
  | MBinary of binop * immediate * immediate
  | MDataOp of data_op * immediate
  (* Bindings to local variables, whole list backpatched together to handle mutual recursion *)
  | MStore of (binding * instr) list
  | MDrop (* Ignore the result of the last expression. Used for sequences. *)

and block = instr list

(* Used to represent the runtime imports *)
type import = {
  mimp_name: Ident.t;
  mimp_type: asmtype list * asmtype list;
}

type export = {
  ex_name: Ident.t;
  ex_global_index: int32;
} 

type binds_function = {
  index: int32;
  arity: int32;
  body: block;
  stack_size: int;
} 

type binds_program = {
  functions: binds_function list;
  exports: export list;
  main_body: block;
  main_body_stack_size: int;
  num_globals: int;
} 

(* Matches values used by IR currently i.e. true -> const 1, false -> const 0 *)
let const_true =  MConstI32 (Int32.of_int 1)
let const_false = MConstI32 (Int32.of_int 0)

type type_tag = Number | Data | Closure
let tag_of_type = function
  | Number -> 0
  | Data -> 1
  | Closure -> 3
