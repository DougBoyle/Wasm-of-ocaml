open Format
open Wasmtree

let print_typ ppf = function
  | I32Type -> fprintf ppf "i32"
  | I64Type -> fprintf ppf "i64"
  | F32Type -> fprintf ppf "f32"
  | F64Type -> fprintf ppf "f64"

let rec print_const ppf = function
  | MConstI32 n -> fprintf ppf "%lil" n
  | MConstI64 n -> fprintf ppf "%LiL" n
  | MConstF32 f -> fprintf ppf "%f" f
  | MConstF64 f -> fprintf ppf "%f" f
  | MConstLiteral c -> fprintf ppf "Literal %a" print_const c

let binary ppf = function
  | Eq -> fprintf ppf "="
  | Neq -> fprintf ppf "<>"
  | LT -> fprintf ppf "<"
  | GT -> fprintf ppf ">"
  | LTE -> fprintf ppf "<="
  | GTE -> fprintf ppf ">="
  | Compare  -> fprintf ppf "compare"
  | Min (* missing *)  -> fprintf ppf "min"
  | Max (* missing *) -> fprintf ppf "max"
  | Eq_phys -> fprintf ppf "=="
  | Neq_phys -> fprintf ppf "!="
  (* boolean *)
  | AND -> fprintf ppf "&&"
  | OR -> fprintf ppf "||"
  (* integer *)
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | Mult -> fprintf ppf "*"
  | Div -> fprintf ppf "/"
  | Mod -> fprintf ppf "mod"
  (* list *)
  | Append -> fprintf ppf "@"

let unary ppf = function
  (* boolean *)
  | Not -> fprintf ppf "not"
  (* integer *)
  | UnNeg -> fprintf ppf "-"
  (* Identity - part of OCaml due to principle of least suprise, just the identity op *)
  | UnAdd -> fprintf ppf "identity"
  | Succ -> fprintf ppf "succ"
  | Pred -> fprintf ppf "pred"
  | Abs (* missing *) -> fprintf ppf "abs"

let print_bind ppf = function
  | MArgBind i -> fprintf ppf "arg(%li)" i
  | MLocalBind i -> fprintf ppf "local(%li)" i
  | MGlobalBind i -> fprintf ppf "global(%li)" i
  | MClosureBind i -> fprintf ppf "free-var(%li)" i
  | MSwapBind i -> fprintf ppf "swap-var(%li)" i

let print_imm ppf = function
  | MImmBinding b -> print_bind ppf b
  | MImmConst c -> print_const ppf c
  | MImmFail n -> fprintf ppf "@[<2>(fail@ %li)@]" n

let print_imms ppf args = List.iter (fun e -> fprintf ppf "@ %a" print_imm e) args

let print_alloc ppf = function
  | MClosure {func_idx; variables} (* Arity ignored *) ->
    fprintf ppf "@[closure(func(%li)@ %a)@]" func_idx print_imms variables
  | MData(i, args) ->
    fprintf ppf "@[<2>(makeblock %li%a)@]" i print_imms args

let rec print_instr ppf = function
    | MImmediate i -> print_imm ppf i
    | MCallKnown(i, args) ->
      let print_args ppf args = List.iter (fprintf ppf " %a" print_imm) args in
      fprintf ppf "(call@ func(%li)%a)" i print_args args
    | MCallIndirect(imm, args) ->
      let print_args ppf args = List.iter (fprintf ppf " %a" print_imm) args in
      fprintf ppf "(call@ %a%a)" print_imm imm print_args args
    | MAllocate alloc -> print_alloc ppf alloc
    | MIf (cond, thn, els) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" print_imm cond print_block thn print_block els
    | MWhile (cond, block) -> fprintf ppf "@[<2>(while@ %a@ %a)@]" print_imm cond print_block block
    | MFor(arg1, start, dir, _, finish, body) ->
     fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
         print_bind arg1 print_imm start
         (match dir with Upto -> "to" | Downto -> "downto")
         print_imm finish print_block body
    | MTry(i, body, handle) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with (%li)@ %a)@]" print_block body i print_block handle
    | MUnary (op, imm) -> fprintf ppf "(%a %a)" unary op print_imm imm
    | MBinary (op, imm1, imm2) -> fprintf ppf "(%a %a %a)" print_imm imm1 binary op print_imm imm2
    | MDataOp (MGet i, imm) -> fprintf ppf "%a.(%lil)" print_imm imm i
    | MDataOp (MSet (i, v), imm) -> fprintf ppf "%a.(%lil) <- %a" print_imm imm i print_imm v
    | MDataOp (MGetTag, imm) -> fprintf ppf "tag %a" print_imm imm
    | MStore binds ->
     let bindings ppf id_arg_list =
      let spc = ref false in
      List.iter
        (fun (bind, instr) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<2>%a@ = %a@]" print_bind bind print_instr instr)
        binds in
     fprintf ppf  "@[<2>(store@ (@[<hv 1>%a@]))@]" bindings binds
    | MDrop -> fprintf ppf "drop"
    | MTagOp(_, _) | MArityOp(_, _, _) -> failwith "Not expecting to see tag/arity operations"

and print_block ppf (block : instr list) =
  let print_body ppf body = List.iter (fprintf ppf " %a" print_instr) body in
  fprintf ppf "@[<2>{@ %a}@]" print_body block

(* TODO: Also print stuff like arity/stack space allocated *)
let print_function ppf {index;body;_} =
  fprintf ppf "@[func(%li) %a@]" index print_block body

let print_program ppf {functions;imports;exports;main_body;_} =
  List.iter (print_function ppf) functions;
  fprintf ppf " @[main %a@]" print_block main_body