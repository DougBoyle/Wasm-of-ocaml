(* Scan through each function in reverse to remove any dead assignments.
   From end of function, a 'get' marks a local as active, a 'set'/'tee' marks it as inactive.
   If a 'set' is reached when inactive, can change to a 'drop' (optimise later), and a 'tee' can just be deleted.
   Also, if there are no 'gets' of a local, it is unused so should be removed (decrement all greater indices accordingly) *)
open Wasm.Ast
open Wasm.Source
(* TODO: Just transform to/from graph (maybe as an annotation on top of Wasm.Ast?) to make checks simpler due to
         branches being reverse-linked *)

module Set32 = Set.Make(Int32)
let enter_block block_list = List.map (fun (i, active) -> (Int32.add i 1l, active)) block_list

(* For working out the varaibles which loops expect to be already set.
   Labels stores the minimum collection of locals guarenteed to have been set when that Block's label is reached
   i.e. nothing after the first branch occurs. *)
let labels = ref ([] : (int32 * (Set32.t ref)) list) (* Should probably be a HashTbl? *)

let set_label i seen =
  match List.assoc_opt i (!labels) with
    | None -> ()
    | Some r -> r := seen; labels := List.filter (fun (j, _) -> i <> j) (!labels)

(* Returns a pair of (got_before_set, set_or_got) to identify which locals are 'active' at the beginning of a loop
   Traverses instructions in forward direction *)
let rec needed depth seen = function
  (* update labels[0] if it is still present i.e. block has no branches (happens in 'if' blocks) *)
  | [] -> set_label depth seen; Set32.empty, seen
  | {it=LocalGet {it=i}}::rest -> let got, new_seen = needed depth (Set32.add i seen) rest in
    if Set32.mem i seen then got, new_seen else Set32.add i got, new_seen
  | {it=(LocalSet {it=i}|LocalTee {it=i})}::rest -> needed depth (Set32.add i seen) rest
  | {it=Block(typ, body)}::rest ->
    let r = ref seen in (* May as well use seen, going to be overwritten anyway *)
    labels := (Int32.add depth 1l, r)::(!labels);
    let block_got, _ = needed (Int32.add depth 1l) seen body in
    let new_seen = !r in (* Superset of seen. Only locals guarenteed to have been set *)
    let new_seen = Set32.union block_got new_seen in
    let got, seen = needed depth new_seen rest in Set32.union block_got got, seen
  | {it=If(typ, body1, body2)}::rest ->
    let r1 = ref seen in
    labels := (Int32.add depth 1l, r1)::(!labels);
    let block_got1, _ = needed (Int32.add depth 1l) seen body1 in
    let r2 = ref seen in
    labels := (Int32.add depth 1l, r2)::(!labels);
    let block_got2, _ = needed (Int32.add depth 1l) seen body2 in
    (* can ignore anything definitely set on both branches *)
    let new_seen = Set32.inter (!r1) (!r2) in
    let new_seen = Set32.union (Set32.union block_got1 block_got2) new_seen in
    let got, seen = needed depth new_seen rest in Set32.union (Set32.union block_got1 block_got2) got, seen
  (* Loops must fully execute so can safely use their result *)
  | {it=Loop(typ, body)}::rest ->
    let body_got, seen = needed (Int32.add depth 1l) seen body in
    let got, seen = needed depth seen rest in Set32.union body_got got, seen
  (* If a branch is seen, update the label it points to as anything later may not actually get set *)
  | ({it=Br {it=i}}|{it=BrIf {it=i}})::rest ->
    set_label (Int32.sub depth i) seen; needed depth seen rest
  | ({it=BrTable (vars, var)})::rest ->
    List.iter (fun {it=i} -> set_label (Int32.sub depth i) seen) (var::vars); needed depth seen rest
  | _::rest -> needed depth seen rest

let get_active i blocks = match List.assoc_opt i blocks with
  | Some active -> active
  | None -> Set32.empty

(* Work backwards to find dead assignments. Since assignments at the end of a loop can be used at the start of them,
   use 'needed' above to check which locals a loop expects to have already been assigned, hence can't remove those. *)
let rec reverse_step blocks active = function
  | [] -> [], active
  | ({it=LocalGet {it=i}} as instr)::rest ->
    let instrs, end_active = reverse_step blocks (Set32.add i active) rest in
    instr::instrs, end_active
  | ({it=LocalTee {it=i}} as instr)::rest  ->
    if Set32.mem i active
    then let instrs, end_active = reverse_step blocks (Set32.remove i active) rest in instr::instrs, end_active
    else reverse_step blocks active rest
  | ({it=LocalSet {it=i}} as instr)::rest -> if Set32.mem i active
    then let instrs, end_active = reverse_step blocks (Set32.remove i active) rest in instr::instrs, end_active
    else let instrs, end_active = reverse_step blocks active rest in {instr with it = Drop}::instrs, end_active
  | ({it=Block(typ, body)} as instr)::rest ->
    let rev_instrs, end_active = reverse_step ((0l, active)::(enter_block blocks)) active (List.rev body) in
    let instrs, end_active2 = reverse_step blocks end_active rest in
    {instr with it=Block(typ, List.rev rev_instrs)}::instrs, end_active2
  | ({it=Loop(typ, body)} as instr)::rest ->
    let required = fst (needed 0l Set32.empty body) in
    (* Because branches to loops just repeat themselves, not added to blocks list *)
    let rev_instrs, end_active = reverse_step blocks (Set32.union active required) (List.rev body) in
    let instrs, end_active2 = reverse_step blocks end_active rest in
    {instr with it=Loop(typ, List.rev rev_instrs)}::instrs, end_active2
  | ({it=If(typ, body1, body2)} as instr)::rest ->
    let rev_instrs1, end_active1 = reverse_step ((0l, active)::(enter_block blocks)) active (List.rev body1) in
    let rev_instrs2, end_active2 = reverse_step ((0l, active)::(enter_block blocks)) active (List.rev body2) in
    (* Union here is conservative *)
    let instrs, end_active = reverse_step blocks (Set32.union end_active1 end_active2) rest in
    {instr with it=If(typ, List.rev rev_instrs1, List.rev rev_instrs2)}::instrs, end_active
  (* Also include any variables required after a the label being jumped to *)
  | (({it=Br {it=i}}|{it=BrIf {it=i}}) as instr)::rest ->
      let instrs, end_active = reverse_step blocks (Set32.union active (get_active i blocks)) rest in
      instr::instrs, end_active
  (* Consider all possible branch targets *)
  | (({it=BrTable (vars, var)}) as instr)::rest ->
      let active = List.fold_right (fun {it=i} active -> Set32.union active (get_active i blocks)) (var::vars) active in
      let instrs, end_active = reverse_step blocks active rest in
      instr::instrs, end_active
  | instr::rest -> let instrs, end_active = reverse_step blocks active rest in instr::instrs, end_active

let optimise_function ({it=func} as phrase) =
  {phrase with it={func with body=List.rev (fst(reverse_step [] Set32.empty (List.rev func.body)))}}

let optimise ({it=wasm_mod} as phrase) =
  {phrase with it={wasm_mod with funcs=List.map optimise_function wasm_mod.funcs}}
