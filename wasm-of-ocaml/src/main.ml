let input  = ref ""
let output = ref ""

let print_ir = ref false
let print_wasmtree = ref false
let print_wat = ref false

(* Prevents .cmi files being generated.
If compiling multiple files together, remove this so cmi's can be used in type checking. *)
let _ = Clflags.dont_write_files := true

let main () =
  let filename = !input in
  let output_prefix = Filename.remove_extension filename in
  let output_dir = if !output = "" then Filename.dirname filename else !output in
  let output_file = Filename.concat output_dir ((Filename.basename output_prefix) ^ ".wasm") in
  let tree, coercions =
  try  Compile.typed_implementation Format.err_formatter filename output_prefix
  with x -> (Location.report_exception Format.err_formatter x; exit 1) in
  let ir = Linearise.translate_structure_with_coercions (tree, coercions) in

  (* Simple analysis pass *)
  PropagateAnalysis.analyse ir;
  (* Attempt to analyse how immediates passed around memory *)
  Analyse.analyse_constant_propagation ir;
  (* Basic optimisation pass, only removes assignments OCaml already warns about until CSE/const propagation added *)
  let ir = OptConstants.optimise ir in
  let ir = OptFails.optimise ir in (* Optimising constants removes dead branches, leaves useless try/catches *)
  let ir = OptCSE.optimise ir in
  let ir = OptDeadAssignments.optimise ir in

  if !print_ir then (Pplinast.print_ast Format.std_formatter ir; Format.print_newline();  Format.print_newline());
  let wasm_ast = Compilebinds.transl_program ir in
  if !print_wasmtree then
    (Ppwasmtree.print_program Format.std_formatter wasm_ast; Format.print_newline(); Format.print_newline());
  (* Now that this produces a graph, should rename as such *)
  let graph = Compilewasm.compile_wasm_module wasm_ast in

  (* Wasm level optimisations *)
  let graph = OptWasmPeephole.optimise graph in
  let graph = Deadlocals.optimise graph in
  let graph = OptWasmDrop.optimise graph in

  let wasm = Graph.translate_to_wasm graph in
  Compilewasm.validate_module wasm;

  let binary = Wasm.Encode.encode wasm in
  let f = open_out_bin output_file in
  output_string f binary; close_out f;
  if !print_wat then Printf.fprintf stdout "%s\n" (Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ wasm))

let _ = Arg.parse [("-d", Arg.Set_string output, "Specify output directory");
                   ("-ir", Arg.Set print_ir, "Print Linast IR program produced");
                   ("-wt", Arg.Set print_wasmtree, "Print Wasmtree produced");
                   ("-wat", Arg.Set print_wat, "Print output wat file")]
    (fun f -> if (!input) = "" then input := f else raise (Arg.Bad "Only one file allowed"))
    "Usage: main.byte [<file>]"

let _ = main ()
