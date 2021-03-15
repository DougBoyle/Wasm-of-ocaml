(* Use live variables set on each node to assign registers more efficiently
   Swap registers are left out for now. TODO: Include these? Complicates analysis of shadow stack size needed
   Argument slots must be kept unchanged, but other values can be stored in them if needed,
     so these are pre-allocated to themselves.
 *)
open Graph

(* TODO: Better graph representation
  current representation is (int32 * (int32 option) * (int32 list)) list
  - possibly want to make it more mutable?
*)
type clashgraph = (Set32.elt * (int32 option) * Set32.elt list) list

(* ------------------------------- Building clash graph from live variables --------------------------- *)
let rec add_edge a b = function
  | [] -> []
  | (n, colour, edges)::rest when n = a ->
    (n, colour, if List.mem b edges then edges else b::edges)::add_edge a b rest
  | (n, colour, edges)::rest when n = b ->
    (n, colour, if List.mem a edges then edges else a::edges)::add_edge a b rest
  | node::rest -> node::add_edge a b rest

let is_not_swap arity num_swaps n =
  let v = Int32.to_int n in
  v < arity || v >= (arity + num_swaps)

let add_edges arity num_swaps live graph  =
  (* written this way to avoid trying to add every edge twice.
     adds an edge a <-> b for every a, b simultaneuously live,
     unless one of them is a swap variable *)
  (* skip over swap variables. Since we avoid putting them into seen,
     don't need to check when removing from seen *)
  snd (Set32.fold (fun node (seen, graph) ->
    if is_not_swap arity num_swaps node then
    (node::seen, List.fold_right
    (add_edge node) seen graph)
    else (seen, graph)) live ([], graph))

(* TODO: At this point could probably do with a map/iter function over the Graph datatype *)

(* add edges between every simultaneously live variable anywhere in the program *)
let rec add_all_edges arity num_swaps body graph =
  List.fold_right (fun instr graph -> match instr.it with
  (* Need to handle recursion on the block structures, otherwise just look at !(instr.live) *)
  | Block (_, body) | Loop (_, body) -> add_all_edges arity num_swaps body graph
  | If (_, body1, body2) ->
    add_all_edges arity num_swaps body2 (add_all_edges arity num_swaps body1 graph)
  (* 'Tee i' is effectively [Set i; Get i] so i is generated then immediately killed (in reverse).
      As such, just treat the same as 'Set i' in live analysis, but need a special case here since
      internally it can still generate clashes between other live variables and i *)
  (* 'Set i' must also always add edges to i, even though the assignment might be dead
     (if optimisations disabled), since it will need to map to something (only removed if opts enabled)
     that is different to anything currently live *)
  | LocalTee i | LocalSet i -> add_edges arity num_swaps (Set32.add i.it (!(instr.live))) graph
  | _ -> add_edges arity num_swaps (!(instr.live)) graph) body graph


(* -------------------------------- Colouring based on clash graph ------------------------------------ *)
let get_smallest_node =
 let rec help node degree = function
   | [] -> node
   | (i, _, edges)::rest -> let n = List.length edges in
     if n < degree then help i n rest else help node degree rest in
 function
  | [] -> failwith "Empty graph, no smallest degree node"
  | (i, _, edges)::rest -> help i (List.length edges) rest

let rec remove_node n = function
  | [] -> []
  | (i, colour, edges)::rest ->
    if i = n then remove_node n rest
    else (i, colour, List.filter (fun j -> j <> n) edges)::remove_node n rest

(* Build stack of order to process nodes in, ignoring any that are already coloured *)
let build_stack graph =
  let uncoloured_graph = List.fold_right
    (fun (i, colour, _) graph -> if colour = None then graph else remove_node i graph) graph graph in
  let rec help stack graph =
    if graph = [] then stack else
    let smallest = get_smallest_node graph in
    help (smallest::stack) (remove_node smallest graph) in
  help [] uncoloured_graph

(* Just for debugging
let print_graph graph =
  List.iter (fun (i, c, l) -> Printf.printf "%li: " i;
    List.iter (fun j -> Printf.printf "%li " j) l; Printf.printf "\n") graph
*)

(* swap slots are left out, so need the arity and num_swaps to correctly generate indicies *)
let colour_node arity num_swaps graph node =
  let neighbours = (fun (_, _, l) -> l) (List.find (fun (i, _, _) -> i = node) graph) in
  let invalid_colours =
    List.fold_right
    (fun n blocked -> match List.find (fun (i, _, _) -> i = n) graph with
      | (_, Some c, _) -> if List.mem c blocked then blocked else c::blocked
      | _ -> blocked) neighbours [] in
  let rec get_colour n =
    (* Need to skip past any swap variables *)
    if n = arity && num_swaps > 0 then get_colour (n + num_swaps)
    else if List.mem (Int32.of_int n) invalid_colours then get_colour (n+1) else Int32.of_int n in
  let colour = get_colour 0 in
  List.fold_right (fun ((n, _, edges) as vertex) output ->
    if n = node then (n, Some colour, edges)::output else vertex::output) graph []

(* Outputs a mapping for all locals, as well as the total number of locals (minus args) now used *)
let allocate_registers arity num_swaps graph =
  let stack = build_stack graph in
  let result = List.fold_right (fun n graph -> colour_node arity num_swaps graph n) stack graph in
  let mapping =
    (* swap variables are handled separately and mapped to themselves *)
    (List.init num_swaps (fun i -> let idx = Int32.of_int (i + arity) in (idx, idx)))
    @ (List.map (function (n, Some m, _) -> (n, m) | _ -> failwith "Node was not coloured") result) in
  let max = List.fold_right (fun (_, n) m -> max n m) mapping (-1l) in
  (mapping, Int32.to_int max)

(* ----------------------- Top level functions ---------------------- *)

let allocate_func_registers types ({ftype; locals; num_swaps; body} as f) =
  let arity = GraphUtils.func_arity types ftype.it in
 (* Printf.printf "arity: %d, swaps: %d, locals: %d\n" arity num_swaps (List.length locals); *)
  let nodes =
    (* arguments get pre-coloured *)
    (List.init arity (fun i -> (Int32.of_int i, Some (Int32.of_int i), []))) @
    (List.init ((List.length locals) - num_swaps)
      (fun i -> (Int32.of_int (i + num_swaps + arity), None, []))) in
  let graph = add_all_edges arity num_swaps body nodes in
  (* Includes a mapping of all swap variables to themselves *)
  let mapping, max_local = allocate_registers arity num_swaps graph in
  (* +1 since first local is index 0. If no locals (including arguments), max = -1 so this still works *)
  {f with locals=List.init (1 + max_local - arity) (fun _ -> Wasm.Types.I32Type);
     body=List.map (Deadlocals.apply_local_mapping mapping) body}

let colour_registers ({types; funcs} as module_) =
  (* Get accurate analysis in case any optimisations have changed this. Reset first *)
  List.iter (fun {body} -> List.iter Deadlocals.reset_liveness body) funcs;
  List.iter (fun {body} -> Deadlocals.analyse_liveness (List.rev body)) funcs;
  {module_ with funcs=List.map (allocate_func_registers types) funcs}
