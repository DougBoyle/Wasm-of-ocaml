open Linast

exception NotImplemented
exception NotSupported (* TODO: Modify earlier parts of OCaml frontend to not accept these elements *)

type linast_setup =
  | BEffect of compound_expr
  | BLet of Ident.t * compound_expr
  | BLetRec of (Ident.t * compound_expr) list
  | BLetExport of Asttypes.rec_flag * (Ident.t * compound_expr) list

(* Primative name -> Ident *)
let primIds : (string * Ident.t) list ref = ref []
(* List of Binds to include in program *)
let primBinds : linast_setup list ref = ref []