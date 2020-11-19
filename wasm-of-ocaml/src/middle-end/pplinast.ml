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

let print_imm ppf (imm : imm_expr) = match imm.desc with
  | ImmIdent id -> Ident.print ppf id
  | ImmConst c -> struct_const ppf c
  | ImmMatchFail n -> fprintf ppf "%lil" n

let rec print_compound ppf (c : compound_expr) = match c.desc with
    | CImm i -> print_imm ppf i
    | CUnary (op, imm) -> fprintf ppf "(%a %a)" unary op print_imm imm
    | CBinary (op, imm1, imm2) -> fprintf ppf "(%a %a %a)" print_imm imm1 binary op print_imm imm2
    | CSetField (imm1, i, imm2) -> fprintf ppf "%a.(%lil) <- %a" print_imm imm1 i print_imm imm2
    | CField (imm, i) -> fprintf ppf "%a.(%lil)" print_imm imm i
    | CMakeBlock (i, imms) ->
      let print_imms ppf args =
        List.iter (fun imm -> fprintf ppf "@ %a" print_imm imm) args in
      fprintf ppf "[%li: %a]" i print_imms imms
    | CGetTag imm -> fprintf ppf "tag %a" print_imm imm
    | CIf (imm, ast1, ast2) -> fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" print_imm imm print_ast ast1 print_ast ast2
    | CWhile (imm, ast) -> fprintf ppf "@[<2>(while@ %a@ %a)@]" print_imm imm print_ast ast
    | CFor (id, start, finish, dir, ast) ->
    fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
           Ident.print id print_imm start
           (match dir with Upto -> "to" | Downto -> "downto")
           print_imm finish print_ast ast

    | CSwitch (imm, cases, partial) ->
      let print_switch ppf (cases,partial) =
        List.iter
         (fun (n, e) ->
           fprintf ppf "case %n: %a" n print_ast e) cases;
        begin match partial with
        | None  -> ()
        | Some e -> fprintf ppf "default: %a" print_ast e
        end in
      fprintf ppf "(switch %a %a)" print_imm imm print_switch (cases, partial)
    | CMatchTry (i, e1, e2) -> fprintf ppf "(try (%lil) %a with %a)" i print_ast e1 print_ast e2
    | CApp (imm, args) -> let print_args ppf args =
                                  List.iter (fun imm -> fprintf ppf " %a" print_imm imm) args in
                                fprintf ppf "(apply@ %a%a)" print_imm imm print_args args
    | CFunction (args, body) ->
 let pr_params ppf params =
            List.iter (fun param ->
                fprintf ppf "@ %a" Ident.print param) params
in
      fprintf ppf "@[<2>(function%a@ %a)@]" pr_params args
             print_ast body

and print_ast ppf e = match e.desc with
  | LCompound c -> print_compound ppf c
  | LSeq (c, e) -> fprintf ppf "%a; %a" print_compound c print_ast e
  | LLet(rec_flag, id_arg_list, body) ->
    let print_bind ppf (id, global, c) =
      fprintf ppf "@[<2>(%s%a@ %a)@]" (match global with Global -> "exported " | Local -> "") Ident.print id print_compound c in
        let bindings ppf id_arg_list =
          let spc = ref false in
          List.iter
            (fun b ->
              if !spc then fprintf ppf "@ " else spc := true;
              print_bind ppf b)
            id_arg_list in
        fprintf ppf
          "@[<2>(let%s@ @[<hv 1>%a@]@ in@ %a)@]" (match rec_flag with Recursive -> "rec " | _ -> "") bindings id_arg_list print_ast body
(*  | LLet (rec_flag, binds, body) ->
    let print_bind ppf (id, global, c) = fprintf ppf "%s%a = %a, " (match global with Global -> "exported " | Local -> " ") Ident.print id print_compound c
    in fprintf ppf "let%s" (match rec_flag with Recursive -> "rec " | _ -> "") ;
    List.iter (fun bind -> print_bind ppf bind) binds;
    fprintf ppf " in %a" print_ast body *)

