open Compilerflags

(* Command line options/arguments *)
let input  = ref ""
let output = ref ""

(* Display intermediate representations *)
let print_ir = ref false
let print_bindstree = ref false
let print_wat = ref false

(* Optimisation settings *)
let opt_ir = ref true
let num_passes_ir = ref 3
let opt_graph = ref true
let num_passes_graph = ref 3

(* Prevents .cmi files being generated.
If compiling multiple files together, remove this so cmi's can be used in type checking. *)
let _ = Clflags.dont_write_files := true

(* Optimisations *)
let ir_analysis = [
  AnalyseBlocks.analyse;
  AnalysePurity.analyse;
]

let ir_passes = [
  ("copy", OptImms.optimise);
  ("const", OptConstants.optimise);
  ("fails", OptFails.optimise);
  ("cse",  OptCSE.optimise);

  (* Copy propagation and CSE reveal additional cases where these can be applied *)
  ("uncurry", OptUncurry.optimise);
  ("inline", OptInline.optimise);
  ("tail calls", OptTailCalls.optimise);

  (* All of the above optimisations may have made some values redundant now so they are removed *)
  ("deadassign", OptDeadAssignments.optimise);
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

  (* Reduce the number of locals used *)
  let graph = Colouring.colour_registers graph in

  (* Now registers have been re-mapped, remove any useless copies 'Get i; Set i' *)
  let graph = if (!opt_graph) then OptWasmPeephole.optimise graph else graph in

  let wasm = OutputWasm.translate_to_wasm graph in
  Compilewasm.validate_module wasm;

  let binary = Wasm.Encode.encode wasm in
  let f = open_out_bin output_file in
  output_string f binary; close_out f;
  if !print_wat then Printf.fprintf stdout "%s\n" (Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ wasm))

let _ = Arg.parse [
    (* Taken from driver/main_args.ml in OCaml compiler *)
    ("-I", Arg.String (fun dir -> Clflags.include_dirs := (dir :: (!(Clflags.include_dirs)))),
      "<dir>  Add <dir> to the list of include directories");
    ("-w", Arg.String (Warnings.parse_options false),
      Printf.sprintf
      "<list>  Enable or disable warnings according to <list>:\n\
      \        +<spec>   enable warnings in <spec>\n\
      \        -<spec>   disable warnings in <spec>\n\
      \        @<spec>   enable warnings in <spec> and treat them as errors\n\
      \     <spec> can be:\n\
      \        <num>             a single warning number\n\
      \        <num1>..<num2>    a range of consecutive warning numbers\n\
      \        <letter>          a predefined set\n\
      \     default setting is %S" Warnings.defaults_w);

    ("-d", Arg.Set_string output, "Specify output directory");
    ("-ir", Arg.Set print_ir, "Print Linast IR program produced");
    ("-bindstree", Arg.Set print_bindstree, "Print Bindstree produced");
    ("-wat", Arg.Set print_wat, "Print output wat file");
    ("-Nopt-ir", Arg.Clear opt_ir, "Disable IR level optimisations");
    ("-passes-ir", Arg.Set_int num_passes_ir, "Set number of IR passes");
    ("-Nopt-graph", Arg.Clear opt_graph, "Disable Graph level optimisations");
    ("-passes-graph", Arg.Set_int num_passes_graph, "Set number of graph passes");
    ("-Nopt-patterns", Arg.Clear use_optimised_pattern_compilation,
      "Disable optimised pattern compilation");
    ("-No-gc", Arg.Set no_gc, "Disable garbage collection");
  ]
  (fun f -> if (!input) = "" then input := f else raise (Arg.Bad "Only one file allowed"))
  "Usage: main.byte [<file>]"

let _ = main ()
