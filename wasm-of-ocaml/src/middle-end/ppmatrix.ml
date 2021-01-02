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

let print_row ppf (patterns, _, _) =
  let ppf = Format.std_formatter in
  let spc = ref false in
  List.iter
    (fun pat ->
      if !spc then fprintf ppf "@ " else spc := true;
      print_pattern ppf pat)
    patterns

let print_matrix mat =
  let ppf = Format.std_formatter in
  List.iter
    (fprintf ppf "@[<hv 1>[%a]@]@." print_row)
    mat