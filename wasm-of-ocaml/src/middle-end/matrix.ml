(* For all representation code + matrix modification utility functions *)

(* Contexts represented as storing suffix in regular order, and prefix in reverse order.
   Therefore, front of each list is around the current point '.' where all operations actually occur. *)

open Typedtree
open Types

type context = (pattern list * pattern list) list
type jump_handlers = ((pattern list list * int32) list)

let rec take n l = if n = 0 then ([], l) else
  match l with [] -> assert false (* Should never happen *)
    | x::xs -> let (h, t) = take (n-1) xs in (x::h, t)

(* Taken from parmatch.ml *)
let add_dummy_data pat_desc =
  {pat_desc; pat_loc = Location.none; pat_extra = [];
   pat_type = Ctype.none; pat_env = Env.empty; pat_attributes = []}

(* Tpat_any pattern for filling in patterns in certain places *)
let omega = add_dummy_data Tpat_any
let omegas n = List.init n (fun _ -> omega)
(* Tpat_construct containing just omega patterns (used by specialisation *)
let empty_construct constructor_desc =
  add_dummy_data
    (Tpat_construct (mknoloc (Longident.Lident (constructor_desc.Types.cstr_name)),
       constructor_desc, omegas constructor_desc.cstr_arity))
let empty_tuple arity =
  add_dummy_data (Tpat_tuple (omegas arity))
let empty_array arity =
  add_dummy_data (Tpat_array(omegas arity))

let initial_context = [([], [omega])]
let initial_handlers total = if total then [] else [([[omega]], -1l)]

(* ----------- pattern manipluation ------------- *)

(* Using lub defined in parmatch.ml. No reason to duplicate this (although fairly simple).
   Rewritten with options rather than Empty *)
let lub_opt (prefix1, fringe1) (prefix2, fringe2) =
  try Some (Parmatch.lubs prefix1 prefix2, Parmatch.lubs fringe1 fringe2)
  with Parmatch.Empty -> None

let lub_pat_opt pat1 pat2 =
  try Some (Parmatch.lub pat1 pat2)
  with Parmatch.Empty -> None
let lub_mat_opt mat1 mat2 =
  try Some (Parmatch.lubs mat1 mat2)
  with Parmatch.Empty -> None

(* ------------ context operations --------------- *)
(* Due to mixture rule using the whole context for the first recursive call, specialisation converts variable
   patterns to patterns of a particular form known when that subcall is reached. i.e. makes the varible pattern more specific *)
type specialise_type =
  | Construct of Types.constructor_description
  | Tuple of int (* arity *)
  | Array of int
  | Constant of Asttypes.constant (* works for both ints and floats (although shouldn't pattern match floats) *)
  | Record of Types.label_description list (* used labels *)

(* constructor is an instance of Types.constructor_description. *)
let rec specialise_ctx_constructor constructor = function
  | (prefix, {pat_desc=Tpat_any|Tpat_var _}::tail)::rows ->
    ((empty_construct constructor)::prefix, (omegas constructor.cstr_arity) @ tail)
    ::(specialise_ctx_constructor constructor rows)
  | (prefix, {pat_desc=Tpat_construct(_, desc, pats)}::tail)::rows
    (* Is this the correct comparison to be performing? *)
     when desc.cstr_tag = constructor.cstr_tag ->
    ((empty_construct constructor)::prefix, pats @ tail)
    ::(specialise_ctx_constructor constructor rows)
  (* Different constructor *)
  | (_, {pat_desc=Tpat_construct(_, _, _)}::_)::rows ->
    specialise_ctx_constructor constructor rows
  | [] -> []
  | _ -> failwith "Cannot apply constructor specialisation" (* Indicates a typing error if this is reached? *)

(* specialisation for tuples is simple as only one constructor and fixed arity *)
let specialise_ctx_tuple arity ctx =
  List.map (function
    | (prefix, {pat_desc=Tpat_any|Tpat_var _}::tail) ->
       (empty_tuple arity)::prefix, (omegas arity) @ tail
    | (prefix, {pat_desc=Tpat_tuple pats}::tail) -> (empty_tuple arity)::prefix, pats @ tail
    | _ -> failwith "Not a tuple pattern, cannot specialise")
  ctx

let rec specialise_ctx_array arity = function
  | (prefix, {pat_desc=Tpat_any|Tpat_var _}::tail)::rows ->
    ((empty_array arity)::prefix, (omegas arity) @ tail)::(specialise_ctx_array arity rows)
  | (prefix, {pat_desc=Tpat_array pats}::tail)::rows when (List.length pats = arity) ->
    ((empty_array arity)::prefix, pats @ tail)::(specialise_ctx_array arity rows)
  | (prefix, {pat_desc=Tpat_array _}::tail)::rows ->
    specialise_ctx_array arity rows
  | [] -> []
  | _ -> failwith "Not an array pattern, cannot specialise"

(* like constructor case, but arity is always 0 and each constant is a distinct constructor *)
let rec specialise_ctx_constant c = function
  | (prefix, ({pat_desc=Tpat_any|Tpat_var _} as pat)::tail)::rows ->
    ({pat with pat_desc = Tpat_constant c}::prefix, tail)::(specialise_ctx_constant c rows)
  | (prefix, ({pat_desc=Tpat_constant d} as pat)::tail)::rows when c = d ->
    (pat::prefix, tail)::(specialise_ctx_constant c rows)
  | (_, {pat_desc=Tpat_constant _}::_)::rows ->
    specialise_ctx_constant c rows
  | [] -> []
  | _ -> failwith "Cannot apply constant specialisation"

let get_record_patterns lbl_list used_lbls =
  List.map (fun used_lbl ->
  match List.find_opt (fun (_, desc, _) -> desc.lbl_pos = used_lbl.lbl_pos) lbl_list with
    | Some (_, _, p) -> p (* field checked by pattern *)
    | None -> omega (* field not checked by this pattern, can be anything *)
  ) used_lbls

(* ordering of labels forced to be the same as lbls_used. Required for collection to work later *)
let specialise_ctx_record lbls_used ctx =
  (* Record matching each used field against _, indicates which fields to collect later *)
  let num_lbls = List.length lbls_used in
  let empty_record_pats = List.map (fun lbl_desc ->
      (mknoloc (Longident.Lident (lbl_desc.lbl_name)), lbl_desc, omega)) lbls_used in
  List.map (function
  | (prefix, ({pat_desc=Tpat_any|Tpat_var _} as pat)::tail) ->
    ({pat with pat_desc = Tpat_record(empty_record_pats, Closed)}::prefix, (omegas num_lbls) @ tail)
  | (prefix, ({pat_desc=Tpat_record (l, closed)} as pat)::tail) ->
    ({pat with pat_desc = Tpat_record (empty_record_pats, closed)}::prefix,
     (get_record_patterns l lbls_used) @ tail)
  | _ -> failwith "Cannot apply record specialisation"
  ) ctx

let specialise_ctx kind ctx = match kind with
  | Construct desc -> specialise_ctx_constructor desc ctx
  | Tuple arity -> specialise_ctx_tuple arity ctx
  | Array arity -> specialise_ctx_array arity ctx
  | Constant c -> specialise_ctx_constant c ctx
  | Record lbls -> specialise_ctx_record lbls ctx

(* Unlike specialise_ctx, collect_ctx doesn't take an extra argument so can process
   each type of constructor in one function *)
let collect_ctx ctx = List.map
 (function
  | (({pat_desc=Tpat_construct(loc, desc, _)} as pat)::prefix, rest) ->
    let pats, others = take desc.cstr_arity rest in
    prefix, {pat with pat_desc=Tpat_construct(loc, desc, pats)}::others
  | (({pat_desc=Tpat_tuple empty_pats} as pat)::prefix, rest) ->
    let pats, others = take (List.length empty_pats) rest in
    prefix, {pat with pat_desc=Tpat_tuple pats}::others
  | (({pat_desc=Tpat_array empty_pats} as pat)::prefix, rest) ->
    let pats, others = take (List.length empty_pats) rest in
    prefix, {pat with pat_desc=Tpat_array pats}::others
  | (({pat_desc=Tpat_constant _} as pat)::prefix, tail) ->
    prefix, pat::tail
  | (({pat_desc=Tpat_record (l, closed)} as pat)::prefix, rest) ->
    let pats, others = take (List.length l) rest in
    let new_record_pats = List.map
      (fun ((loc, desc, _), pat) -> (loc, desc, pat)) (List.combine l pats) in
    prefix, {pat with pat_desc=Tpat_record (new_record_pats, closed)}::others
  | _ -> failwith "Cannot collect context when prefix doesn't end in some form of constructor"
 ) ctx

let push_ctx ctx =
  List.map (function (prefix, p::tail) -> (p::prefix, tail) | _ -> failwith "Cannot push empty fringe") ctx

let pop_ctx ctx =
  List.map (function (p::prefix, tail) -> (prefix, p::tail) | _ -> failwith "Cannot pop empty prefix") ctx

(* Only used for contexts, where ordering of rows is irrelevant *)
let rec remove_redundant_rows acc = function
  | [] -> acc
  | ((prefix, fringe) as row)::rows ->
    remove_redundant_rows
    (if List.exists
      (fun (prefix', fringe') -> Parmatch.le_pats (prefix' @ fringe') (prefix @ fringe))
        (acc @ rows) then acc else row::acc)
    rows

let rec reset_col i = function
  | [] -> failwith "fringe index out of range"
  | p::ps -> if i = 0 then omega :: ps else p::(reset_col (i-1) ps)

let union_ctx ctx1 ctx2 =
  (* simplification/approximation if context grows too large, paper suggests a max size of 32 rows *)
  (* Unaware of any heuristic for choosing which columns to reset, so just work through fringe.
     Prefix avoided due to needing to keep constructors used for collection later. Could detect
     whole columns of all the same constructor and omega patterns, but leave for now unless it proves necessary *)
  (* Contexts getting too large is only an issue in specific cases.
     If can't simplify any further without looking at prefix, just give up and return the context as is *)
  let rec simplify_fringe i ctx =
    (* small enough to return *)
    if List.length ctx < 32 then ctx else
    (* first try to remove redundant rows *)
    let ctx = remove_redundant_rows [] ctx in
    if List.length ctx < 32 then ctx else
    (* Can reset a column *)
    if i < List.length (snd (List.hd ctx)) then
    simplify_fringe (i+1) (List.map (fun (prefix, fringe) -> (prefix, reset_col i fringe)) ctx)
    else ctx (* give up trying to simplify *)
  in simplify_fringe 0 (ctx1 @ ctx2)

(* Potential explosion in number of rows? *)
(* Over all pairs of rows from each context, keep lub of rows whenever compatible *)
let intersect_ctx ctx1 ctx2 =
  List.fold_right (fun row1 ctx ->
    List.fold_right (fun row2 ctx -> match lub_opt row1 row2 with Some row -> row::ctx | None -> ctx)
    ctx2 ctx) ctx1 []

(* Work out the context when the constant/constructor which occurred was not one of the patterns *)
let extract_ctx pat ctx =
  let (prefix, fringe) = List.hd ctx in
  intersect_ctx [(omegas (List.length prefix), pat :: (omegas ((List.length fringe) - 1)))] ctx

(* ------------------ equivalent operations on jump summaries ------------- *)
(* Jump summary is a list of (i, ctx) pairs for each handler the output could jump to *)
(* push/specialise not needed as just summaries are only ever returned out of procedure *)

let collect_jump_summary jumps =
 List.map (fun (i, ctx) -> (i, collect_ctx ctx)) jumps

let pop_jump_summary jumps =
  List.map (fun (i, ctx) -> (i, pop_ctx ctx)) jumps

(* Some work required to collect union of keys for jumps *)
let union_jump_summary jumps1 jumps2 =
  let shared_jumps = List.map
    (fun (i, ctx1) -> match List.assoc_opt i jumps2 with
      | None -> (i, ctx1) | Some ctx2 -> (i, union_ctx ctx1 ctx2)) jumps1 in
  (* include any key that are in jumps2 but not jumps1 *)
  (List.filter (fun (i, _) -> not (List.mem_assoc i jumps1)) jumps2) @ shared_jumps

(* --------------- equivalent operations on matrices ------------- *)
let mat_to_ctx mat = List.map (fun row -> ([], row)) mat
let ctx_to_mat ctx = List.map snd ctx

(* Rewriting is more efficient, especially in simple cases. How great an actual benefit though? *)
let specialise_matrix kind mat =
  ctx_to_mat (specialise_ctx kind (mat_to_ctx mat))

(* mat needed to avoid _weak type error *)
let push_matrix mat = List.map (List.tl) mat

let intersect_matrix mat1 mat2 =
  ctx_to_mat (intersect_ctx (mat_to_ctx mat1) (mat_to_ctx mat2))

let extract_matrix pat mat =
  intersect_matrix [pat :: (omegas ((List.length (List.hd mat)) - 1))] mat

(* -------------- equivalent operations on reachable trap handlers ------------- *)
(* Reachable trap handlers is list of (pattern matrix, handler int) for where to go if patttern fails *)
let specialise_handlers constructor handlers =
  List.map (fun (mat, i) -> (specialise_matrix constructor mat, i)) handlers

(* mat needed to avoid _weak type error *)
let push_handlers handlers = List.map (fun (mat, i) -> (push_matrix mat, i)) handlers

let extract_handlers pat handlers =
  (* Note that this could actually result in (i, []), then entry could be removed *)
  List.map (fun (mat, i) -> (extract_matrix pat mat, i)) handlers

(* ------------------- Other utilities ------------------ *)

(* Requires the env from pattern.pat_env *)
let lookup_constructors signature example_pat =
  match signature.Types.cstr_res.desc with
    | Tconstr (path,_,_) ->
       (* find_type_descrs returns a pair of constructor_descrs and label_descrs *)
       fst (Env.find_type_descrs path example_pat.pat_env)
    | _ -> failwith "attempted to lookup constructor descriptions for non-constructor pattern"

let make_constructor_pattern example_pat desc =
  Parmatch.pat_of_constr example_pat desc

(* Parmatch.orify is not exported by mli, didn't want to start changing mli files of compiler *)
let make_or_pattern pat1 pat2 =
  {pat_desc = Tpat_or (pat1, pat2, None); pat_loc = Location.none; pat_extra = [];
   pat_type = pat1.pat_type ; pat_env = pat1.pat_env;
   pat_attributes = [];
  }
