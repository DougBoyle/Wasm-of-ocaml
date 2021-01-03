open Linast
open Typedtree
open Format

let rec print_pattern ppf pat = match pat.pat_desc with
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var (s,_) -> fprintf ppf "x"
  | Tpat_alias (p, s,_) -> fprintf ppf "alias(%a)" print_pattern p
  | Tpat_constant (c) -> fprintf ppf "c"
  | Tpat_tuple (l) -> fprintf ppf "(...)"
  | Tpat_construct (li, _, po) -> fprintf ppf "Constr(...)"
  | Tpat_variant (l, po, _) -> fprintf ppf "variant"
  | Tpat_record (l, _c) -> fprintf ppf "{...}"
  | Tpat_array (l) -> fprintf ppf "[|...|]"
  | Tpat_or _ -> fprintf ppf "_"
  | _ -> () (* unsupported pattern *)

let print_row ppf patterns =
  let ppf = Format.std_formatter in
  let spc = ref false in
  List.iter
    (fun pat ->
      if !spc then fprintf ppf "@ " else spc := true;
      print_pattern ppf pat)
    patterns

let print_matrix mat =
  let ppf = Format.std_formatter in
  List.iter (fun (patterns, _, _) ->
    fprintf ppf "@[<hv 1>[%a]@]@." print_row patterns)
    mat

let print_let_matrix mat =
  let ppf = Format.std_formatter in
  List.iter (fun (patterns, _) ->
    fprintf ppf "@[<hv 1>[%a]@]@." print_row patterns)
    mat

let print_context ctx =
  let ppf = Format.std_formatter in
  List.iter (fun (prefix, fringe) ->
      fprintf ppf "@[<hv 1>[%a]@ [%a]@]@." print_row prefix print_row fringe)
      ctx