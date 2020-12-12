open Format
open Wasmtree

let print_typ ppf = function
  | I32Type -> fprintf ppf "i32"
  | I64Type -> fprintf ppf "i64"
  | F32Type -> failwith "Should never have 32-bit float in OCaml"
  | F64Type -> fprintf ppf "f64"

let rec print_const ppf = function
  | MConstI32 n -> fprintf ppf "%lil" n
  | MConstI64 n -> fprintf ppf "%LiL" n
  | MConstF64 f -> fprintf ppf "%f" f

let binary = Pplinast.binary
let unary = Pplinast.unary

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
    | MWhile (cond, block) -> fprintf ppf "@[<2>(while@ %a@ %a)@]" print_block cond print_block block
    | MFor(arg1, start, dir, _, finish, body) ->
     fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
         print_bind arg1 print_imm start
         (match dir with Upto -> "to" | Downto -> "downto")
         print_imm finish print_block body
    | MSwitch(arg, cases, default) ->
        let switch ppf cases =
          let spc = ref false in
          List.iter
            (fun (n, e) ->
              if !spc then fprintf ppf "@ " else spc := true;
              fprintf ppf "@[<hv 1>case %li:@ %a@]" n print_block e)
            cases ;
           if !spc then fprintf ppf "@ " else spc := true;
              fprintf ppf "@[<hv 1>default:@ %a@]" print_block default
           in
        fprintf ppf
         "@[<1>(switch %a@ @[<v 0>%a@])@]"
         print_imm arg switch cases
    | MTry(i, body, handle) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with (%li)@ %a)@]" print_block body i print_block handle
    | MUnary (op, imm) -> fprintf ppf "(%a %a)" unary op print_imm imm
    | MBinary (op, imm1, imm2) -> fprintf ppf "(%a %a %a)" print_imm imm1 binary op print_imm imm2
    | MDataOp (MGet i, imm) -> fprintf ppf "%a.(%lil)" print_imm imm i
    | MDataOp (MSet (i, v), imm) -> fprintf ppf "%a.(%lil) <- %a" print_imm imm i print_imm v
    | MDataOp (MGetTag, imm) -> fprintf ppf "tag %a" print_imm imm
    | MDataOp (MArrayGet i, imm) -> fprintf ppf "%a.(%a)" print_imm imm print_imm i
    | MDataOp (MArraySet (i, v), imm) -> fprintf ppf "%a.(%a) <- %a" print_imm imm print_imm i print_imm v
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
    let print_body ppf instrs =
        let spc = ref false in
        List.iter
          (fun instr ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@]" print_instr instr)
          instrs in
      fprintf ppf  "@[<2>(@[<hv 1>%a@])@ @]" print_body block

let print_program ppf {functions;exports;main_body;main_body_stack_size} =
  let print_functions ppf funs =
          let spc = ref false in
          List.iter
            (fun {index;arity;body;stack_size} ->
              if !spc then fprintf ppf "@ " else spc := true;
              fprintf ppf "@[<2>func(%li)@ (%li args, %d locals) %a@.@]" index arity stack_size print_block body)
            funs in
        fprintf ppf
          "@[<2>%a@[<hv>main(%d locals) %a@]@]"
          print_functions functions main_body_stack_size print_block main_body
