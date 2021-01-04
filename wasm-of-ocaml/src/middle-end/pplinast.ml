(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Asttypes
open Primitive
open Types
open Linast


let rec struct_const ppf = function
  | Const_int n -> fprintf ppf "%i" n
  | Const_char c -> fprintf ppf "%C" c
  | Const_string (s, _, _) -> fprintf ppf "%S" s
  | Const_float f -> fprintf ppf "%s" f
  | Const_int32 n -> fprintf ppf "%lil" n
  | Const_int64 n -> fprintf ppf "%LiL" n
  | Const_nativeint n -> fprintf ppf "%nin" n


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
  (* float *)
  | FAdd -> fprintf ppf "+."
  | FSub -> fprintf ppf "-."
  | FMult -> fprintf ppf "*."
  | FDiv -> fprintf ppf "/."

let unary ppf = function
  (* boolean *)
  | Not -> fprintf ppf "not"
  (* integer *)
  | UnNeg -> fprintf ppf "-"
  (* Identity - part of OCaml due to principle of least suprise, just the identity op *)
  | UnAdd -> fprintf ppf "identity"
  | Succ -> fprintf ppf "succ"
  | Pred -> fprintf ppf "pred"
  | Abs -> fprintf ppf "abs"
  (* float *)
  | FUnNeg -> fprintf ppf "-."
  | FSqrt -> fprintf ppf "sqrt"

let print_imm ppf (imm : imm_expr) = match imm.desc with
  | ImmIdent id -> Ident.print ppf id
  | ImmConst c -> struct_const ppf c

let rec print_compound ppf (c : compound_expr) = match c.desc with
    | CImm i -> print_imm ppf i
    | CMatchFail n -> fprintf ppf "@[<2>(fail@ %li)@]" n
    | CUnary (op, imm) -> fprintf ppf "(%a %a)" unary op print_imm imm
    | CBinary (op, imm1, imm2) -> fprintf ppf "(%a %a %a)" print_imm imm1 binary op print_imm imm2
    | CSetField (imm1, i, imm2) -> fprintf ppf "%a.(%i) <- %a" print_imm imm1 i print_imm imm2
    | CField (imm, i) -> fprintf ppf "%a.(%i)" print_imm imm i
    | CArraySet (ar, i, v) -> fprintf ppf "array %a.(%a) <- %a" print_imm ar print_imm i print_imm v
    | CArrayGet (ar, i) -> fprintf ppf "array %a.(%a)" print_imm ar print_imm i
    | CMakeBlock (i, imms) ->
      let print_imms ppf args =
        List.iter (fun e -> fprintf ppf "@ %a" print_imm e) args in
      fprintf ppf "@[<2>(makeblock %i%a)@]" i print_imms imms
    | CGetTag imm -> fprintf ppf "tag %a" print_imm imm
    | CIf (imm, ast1, ast2) -> fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" print_imm imm print_ast ast1 print_ast ast2
    | CWhile (cond, body) -> fprintf ppf "@[<2>(while@ %a@ %a)@]" print_ast cond print_ast body
    | CFor (id, start, finish, dir, ast) ->
    fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
           Ident.print id print_imm start
           (match dir with Upto -> "to" | Downto -> "downto")
           print_imm finish print_ast ast
    | CSwitch(imm, cases, default) ->
          let switch ppf cases =
            let spc = ref false in
            List.iter
              (fun (n, e) ->
                if !spc then fprintf ppf "@ " else spc := true;
                fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n print_ast e)
              cases ;
            begin match default with
            | None  -> ()
            | Some e ->
                if !spc then fprintf ppf "@ " else spc := true;
                fprintf ppf "@[<hv 1>default:@ %a@]" print_ast e
            end in
          fprintf ppf
           "@[<1>(%s %a@ @[<v 0>%a@])@]"
           (match default with None -> "switch*" | _ -> "switch")
           print_imm imm switch cases

    | CMatchTry (i, e1, e2) -> fprintf ppf "@[<2>(try@ %a@;<1 -1>with (%li)@ %a)@]" print_ast e1 i print_ast e2
    | CApp (imm, args) ->
      let print_args ppf args = List.iter (fprintf ppf " %a" print_imm) args in
      fprintf ppf "(apply@ %a%a)" print_imm imm print_args args
    | CFunction (args, body) ->
     let pr_params ppf params = List.iter (fprintf ppf "@ %a" Ident.print) params in
     fprintf ppf "@[<2>(function%a@ %a)@]" pr_params args print_ast body

and print_ast ppf e = match e.desc with
  | LCompound c -> print_compound ppf c
  | LSeq (c, e) -> fprintf ppf "@[<2>(seq@ %a@ %a)@]" print_compound c sequence e
  | LLet(id, export, e, body) ->
      let rec letbody e = match e.desc with
        | LLet(id, export, e, body) ->
            fprintf ppf "@ @[<2>%s%a =@ %a@]"
              (match export with Export -> "export " | Local -> "") Ident.print id print_compound e;
            letbody body
        | _ -> e in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%s%a =@ %a@]"
        (match export with Export -> "export " | Local -> "") Ident.print id print_compound e;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" print_ast expr
  | LLetRec (id_arg_list, body) ->
   let bindings ppf id_arg_list =
    let spc = ref false in
    List.iter
      (fun (id, export, e) ->
        if !spc then fprintf ppf "@ " else spc := true;
        fprintf ppf "@[<2>%s%a@ %a@]" (match export with Export -> "export " | Local -> "") Ident.print id print_compound e)
      id_arg_list in
   fprintf ppf  "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list print_ast body

and sequence ppf e = match e.desc with
  | LSeq (comp, ast) ->
    fprintf ppf "%a@ %a" print_compound comp sequence ast
  | _ ->
    print_ast ppf e