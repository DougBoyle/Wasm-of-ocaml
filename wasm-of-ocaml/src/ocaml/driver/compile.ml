(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Compile_common
open Compenv

let tool_name = "ocamlc"

(** Bytecode compilation backend for .ml files. *)


let typed_implementation ppf sourcefile outputprefix =
    Compmisc.init_path ();
    let modulename = module_of_filename sourcefile outputprefix in
    Env.set_unit_name modulename;
    let env = Compmisc.initial_env() in
    Typemod.type_implementation sourcefile outputprefix modulename env
	(Pparse.parse_implementation ~tool_name sourcefile)
    
