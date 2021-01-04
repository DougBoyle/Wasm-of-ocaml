open Graph
open Format

(*
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

let print_imms ppf args = List.iter (fun e -> fprintf ppf "@ %a" print_imm e) args

let print_alloc ppf = function
  | MClosure {func_idx; variables} (* Arity ignored *) ->
    fprintf ppf "@[closure(func(%li)@ %a)@]" func_idx print_imms variables
  | MData(i, args) ->
    fprintf ppf "@[<2>(makeblock %li%a)@]" i print_imms args
*)

let print_live ppf instr =
  let spc = ref false in
  Set32.iter (fun i -> if !spc then fprintf ppf " " else spc := true;
   fprintf ppf "%li" i) (!(instr.live))

let print_next ppf instr =
  let spc = ref false in
    List.iter (fun instr -> if !spc then fprintf ppf " " else spc := true;
     fprintf ppf "%d " instr.id) (!(instr.succ))

let rec print_instr ppf instr = match instr.it with
  | Unreachable -> fprintf ppf "%d:Unreachable [%a] {next: %a}" instr.id print_live instr print_next instr
  | Nop -> fprintf ppf "%d:Nop [%a] {next: %a}" instr.id print_live instr print_next instr
  | Drop -> fprintf ppf "%d:Drop [%a] {next: %a}" instr.id print_live instr print_next instr
  | Select -> fprintf ppf "%d:Select [%a] {next: %a}" instr.id print_live instr print_next instr
  | Block (_, instrs) -> fprintf ppf "@[<2>(block@ %a)@]" print_block instrs
  | Loop (_, instrs) -> fprintf ppf "@[<2>(loop@ %a)@]" print_block instrs
  | If (_, body1, body2) -> fprintf ppf "@[<2>(if@ then@ %a@ else@  %a)@]" print_block body1 print_block body2
  | Br _ -> fprintf ppf "%d:Br [%a]" instr.id print_live instr
  | BrIf _ -> fprintf ppf "%d:BrIf [%a]" instr.id print_live instr
  | BrTable _ -> fprintf ppf "%d:BrTable [%a]" instr.id print_live instr
  | Return -> fprintf ppf "%d:Return [%a]" instr.id print_live instr
  | Call _ -> fprintf ppf "%d:Call [%a]" instr.id print_live instr
  | CallIndirect _ -> fprintf ppf "%d:CallIndirect [%a]" instr.id print_live instr
  | LocalGet {it=i} -> fprintf ppf "%d:LocalGet %li [%a] {next: %a}" instr.id i print_live instr print_next instr
  | LocalSet {it=i} -> fprintf ppf "%d:LocalSet %li [%a] {next: %a}" instr.id i print_live instr print_next instr
  | LocalTee {it=i} -> fprintf ppf "%d:LocalTee %li [%a] {next: %a}" instr.id i print_live instr print_next instr
  | GlobalGet {it=i} -> fprintf ppf "%d:GlobalGet %li [%a] {next: %a}" instr.id i print_live instr print_next instr
  | GlobalSet {it=i} -> fprintf ppf "%d:GlobalSet %li [%a] {next: %a}" instr.id i print_live instr print_next instr
  | Load _ -> fprintf ppf "%d:Load [%a] {next: %a}" instr.id print_live instr print_next instr
  | Store _ -> fprintf ppf "%d:Store [%a] {next: %a}" instr.id print_live instr print_next instr
  | MemorySize -> fprintf ppf "%d:MemorySize [%a] {next: %a}" instr.id print_live instr print_next instr
  | MemoryGrow -> fprintf ppf "%d:MemoryGrow [%a] {next: %a}" instr.id print_live instr print_next instr
  | Const _ -> fprintf ppf "%d:Const [%a] {next: %a}" instr.id print_live instr print_next instr
  | Test _ -> fprintf ppf "%d:Test [%a] {next: %a}" instr.id print_live instr print_next instr
  | Compare _ -> fprintf ppf "%d:Compare [%a] {next: %a}" instr.id print_live instr print_next instr
  | Unary _ -> fprintf ppf "%d:Unary [%a] {next: %a}" instr.id print_live instr print_next instr
  | Binary _ -> fprintf ppf "%d:Binary [%a] {next: %a}" instr.id print_live instr print_next instr
  | Convert _ -> fprintf ppf "%d:Convert [%a] {next: %a}" instr.id print_live instr print_next instr

and print_block ppf (block : instr list) =
    let print_body ppf instrs =
        let spc = ref false in
        List.iter
          (fun instr ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@]" print_instr instr)
          instrs in
      fprintf ppf  "@[<2>(@[<v>%a@])@ @]" print_body block

let print_program ppf {funcs} =
  let print_functions ppf funs =
          let spc = ref false in
          List.iter
            (fun {body} ->
              if !spc then fprintf ppf "@ " else spc := true;
              fprintf ppf "@[<2>func(...)@ %a@.@]" print_block body)
            funs in
        fprintf ppf
          "@[<2>%a@]"
          print_functions funcs
