(* Command line options/arguments *)
let input  = ref ""
let output = ref ""

(* Display intermediate representations *)
let print_ir = ref false
let print_bindstree = ref false
let print_wat = ref false

(* Optimisation settings *)
let opt_ir = ref true
let num_passes_ir = ref 5 (* TODO: Determine best choice *)
let opt_graph = ref true
let num_passes_graph = ref 5

(* Prevents .cmi files being generated.
If compiling multiple files together, remove this so cmi's can be used in type checking. *)
let _ = Clflags.dont_write_files := true

(* Optimisations *)
(* With this order, analysis of fields won't be lifted up to other terms.
   analyse_constants doesn't rely on pure/immutable annotations so should be done first?
   TODO: Write some test programs and figure this out! *)
let ir_analysis = [
  AnalyseFields.analyse_constant_propagation;
  AnalyseTags.analyse_tags;
  AnalysePurity.analyse;
]
let ir_passes = [
  ("const", OptConstants.optimise);
  ("fails", OptFails.optimise);
  ("cse",  OptCSE.optimise);
  ("deadassign", OptDeadAssignments.optimise);
  (* CSE makes tail calls obvious, deadassign removes pointless functions *)
 (* ("tail", OptTailCalls.optimise);
  ("mututal_tail", OptMutualTails.optimise); *)
 (* ("tail_v2", OptTails2.optimise); *)
  ("tail_v3", OptTails3.optimise);
  ("clear", ClearAnnotations.clear); (* Ready for next analysis pass *)
]

let optimise_ir program =
  if !opt_ir then
    let rec optimise program = function
      | 0 -> program
      | n ->
        (* Analyse purity + how constants are passed around *)
        List.iter (fun analyse -> analyse program) ir_analysis;
        optimise (List.fold_left (fun program (name, optimise) ->
        optimise program) program ir_passes) (n-1) in
    optimise program (!num_passes_ir)
  else program

let graph_passes = [
   OptGlobals.optimise;
   OptWasmPeephole.optimise;
   Deadlocals.optimise;
   OptWasmDrop.optimise
]
let optimise_graph program =
  if !opt_graph then
    let rec optimise program = function
      | 0 -> program
      | n -> optimise (List.fold_left (fun program optimise -> optimise program) program graph_passes) (n-1)
    in optimise program (!num_passes_graph)
  else program

let main () =
  let filename = !input in
  let output_prefix = Filename.remove_extension filename in
  let output_dir = if !output = "" then Filename.dirname filename else !output in
  let output_file = Filename.concat output_dir ((Filename.basename output_prefix) ^ ".wasm") in
  let tree, coercions =
  try  Compile.typed_implementation Format.err_formatter filename output_prefix
  with x -> (Location.report_exception Format.err_formatter x; exit 1) in
  let ir = Linearise.translate_structure_with_coercions (tree, coercions) in

  (* Perform IR level optimisations *)
  let ir = optimise_ir ir in

  if !print_ir then (Pplinast.print_ast Format.std_formatter ir; Format.print_newline();  Format.print_newline());
  let wasm_ast = Compilebinds.transl_program ir in
  if !print_bindstree then
    (Ppbindstree.print_program Format.std_formatter wasm_ast; Format.print_newline(); Format.print_newline());
  (* Now that this produces a graph, should rename as such *)
  let graph = Compilewasm.compile_wasm_module wasm_ast in

  (* Wasm level optimisations *)
  let graph = optimise_graph graph in

  let wasm = Graph.translate_to_wasm graph in
  Compilewasm.validate_module wasm;

  let binary = Wasm.Encode.encode wasm in
  let f = open_out_bin output_file in
  output_string f binary; close_out f;
  if !print_wat then Printf.fprintf stdout "%s\n" (Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ wasm))

let _ = Arg.parse [
    ("-d", Arg.Set_string output, "Specify output directory");
    ("-ir", Arg.Set print_ir, "Print Linast IR program produced");
    ("-bindstree", Arg.Set print_bindstree, "Print Bindstree produced");
    ("-wat", Arg.Set print_wat, "Print output wat file");
    ("-Nopt-ir", Arg.Clear opt_ir, "Disable IR level optimisations");
    ("-passes-ir", Arg.Set_int num_passes_ir, "Set number of IR passes");
    ("-Nopt-graph", Arg.Clear opt_graph, "Disable Graph level optimisations");
    ("-passes-graph", Arg.Set_int num_passes_graph, "Set number of graph passes");
    ("-Nopt-patterns", Arg.Clear Linearise.use_optimised_pattern_compilation, "Disable optimised pattern compilation");
  ]
  (fun f -> if (!input) = "" then input := f else raise (Arg.Bad "Only one file allowed"))
  "Usage: main.byte [<file>]"

let _ = main ()
