let input  = ref ""

(* Prevents .cmi files being generated.
If compiling multiple files together, remove this so cmi's can be used in type checking. *)
let _ = Clflags.dont_write_files := true

let main () =
  let filename = !input in
  let output_prefix = Compenv.output_prefix filename in
  try
    let (tree, coercions) = Compile.typed_implementation Format.err_formatter filename output_prefix in
    Printtyped.implementation_with_coercion Format.std_formatter (tree, coercions);
    let ir = Linearise.translate_structure_with_coercions (tree, coercions) in

    (* For comparison *)
    let lambdaProgram = Translmod.transl_implementation filename (tree, coercions) in

    Pplinast.print_ast Format.std_formatter ir; Format.print_newline();
    Printf.printf "\nLambda:\n";
    Printlambda.program Format.std_formatter lambdaProgram; Format.print_newline();
  with x -> Location.report_exception Format.err_formatter x

let _ = Arg.parse []
    (fun f -> if (!input) = "" then input := f else raise (Arg.Bad "Only one file allowed"))
    "Usage: main.byte [<file>]"

let _ = main ()
