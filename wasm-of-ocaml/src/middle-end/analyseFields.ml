(* Propagate any analysis such as pure fields/expressions from compound -> ident -> compound using that ident *)
(* Attempt to write more general helper function that propagateAnalysis, for specific forms of analysis
  (some work wasted in checking if just the annotation we care about is present or not) *)
(* For now, specialised to handling propagating Imms through memory *)
open Linast

(* Assume working with just one annotation at a time *)
let ident_table = (Ident.Tbl.create 50 : annotation Ident.Tbl.t)

let find_annot_opt annotations =
  List.find_opt (function FieldImms _ -> true | _ -> false) (!annotations)

(* Always replaces any previous annotation. Wasteful but likely ensures correct.
   Reference updated so anything storing a pointer to the same place is also updated. *)
(* Assumes at most 1 of each annotation present in any list *)
let add_annotation annot annotations =
  annotations := annot::(List.filter (function FieldImms _ -> false | _ -> true) (!annotations))

let add_annotation_opt annot annotations = match annot with
  | None -> () | Some annot -> add_annotation annot annotations

(* Updates to_annots with the relevant one from from_annots if it is present *)
let copy_annotation from_annots to_annots =
  match find_annot_opt from_annots with None -> ()
    | Some annot -> add_annotation annot to_annots

let merge_fieldimms = function
  | (None, _) | (_, None) -> None
  | (Some (FieldImms l1), Some (FieldImms l2)) ->
    (* Possibly even different lengths *)
    let len = max (List.length l1) (List.length l2) in
    let new_list = List.init len
    (fun i -> match List.nth l1 i, List.nth l2 i with
      | (None, _) | (_, None) -> None
      | (Some imm1, Some imm2) -> if imm1.desc = imm2.desc then Some imm1 else None) in
    (* Check there is actually a point in having this annotation still *)
    if List.for_all (function None -> true | _ -> false) new_list then None else Some (FieldImms new_list)
  | _ -> failwith "Filter for only fieldImms annotation failed"

(* general way of describing how to merge 2 annotations from branches into a result.
   For constant propagation, this is just taking intersect. *)
let merge_annots from1 from2 =
  merge_fieldimms (find_annot_opt from1, find_annot_opt from2)

(* Updates the annotations on imm *)
let analyse_imm (imm : imm_expr) = match imm.desc with
  | ImmIdent id -> (match Ident.Tbl.find_opt ident_table id with
    (* Update the annotations on that ImmIdent with the tracked annotation *)
    | Some annot -> add_annotation annot imm.annotations
    | None -> ())
  | _ -> ()

(* Currently very restricted in the type of analysis it allows performing, needs rewriting in future. *)
(* Just using for constants for now *)
let rec analyse_compound (compound : compound_expr) = match compound.desc with
  (* Copy up annotations *)
  | CImm imm ->
    analyse_imm imm;
    copy_annotation imm.annotations compound.annotations;

  | CMakeBlock (_, imms) ->
    List.iter analyse_imm imms;
    List.iter (function ImmutableBlock l ->
      let imm_list = List.mapi (fun idx (imm : imm_expr) -> if List.nth l idx then Some imm else None) imms in
      if List.exists (function Some _ -> true | _ -> false) imm_list
        then add_annotation (FieldImms imm_list) compound.annotations | _ -> ()) (!(compound.annotations))

  (* Do actual changing of tree structure in optConstants so that this can just be a side-effect function *)
  (* Separates analysis from optimisations *)

  (* Take the intersection of the annotation on each linast expression *)
  | CIf (_, body1, body2) ->
    analyse_linast body1;
    analyse_linast body2;
    add_annotation_opt (merge_annots body1.annotations body2.annotations) compound.annotations

  (* result is always unit so, for constants optimisations, can't do anything *)
  | CWhile (cond, body) ->
    analyse_linast cond;
    analyse_linast body
  | CFor (_, _, _, _, body) ->
    analyse_linast body

  | CSwitch (_, cases, default) ->
    List.iter (fun (_, body) -> analyse_linast body) cases;
    (match default with Some body -> analyse_linast body | None -> ());
    let body, rest = (match default with Some body -> body, cases
      | None -> match cases with (_, body)::cases -> body, cases | _ -> failwith "Empty match") in
    (* TODO: Rewrite merge function to accept list rather than always pairs. Makes this much neater *)
    add_annotation_opt
    (List.fold_left (fun opt_annot (_, linast) -> merge_annots
      (match opt_annot with None -> ref [] | Some a -> ref [a]) linast.annotations)
      (find_annot_opt body.annotations) rest) compound.annotations

  | CMatchTry (_, body, handle) ->
    analyse_linast handle;
    analyse_linast body;
    (* Unclear if anything more can be assumed about result without greater analysis *)
    add_annotation_opt (merge_annots body.annotations handle.annotations) compound.annotations

  | CFunction (_, body) ->
    analyse_linast body

  | _ -> ()

and analyse_linast linast = match linast.desc with
  (* Add analysis of binds to ident_table. Set overall linast analysis to combination of bind/body *)
  | LLet(id, mut, bind, body) ->
    analyse_compound bind;
    if mut <> Mut then (match find_annot_opt bind.annotations with None -> ()
     | Some annot -> Ident.Tbl.add ident_table id annot);
    analyse_linast body;
    copy_annotation body.annotations linast.annotations

  | LLetRec(_, body) ->
    (* Each bind is a function so, for analysis of compounds, no use processing *)
    analyse_linast body;
    copy_annotation body.annotations linast.annotations
  (* compound result gets ignored so no point evaluating *)
  | LSeq (compound, body) ->
    analyse_linast body;
    copy_annotation body.annotations linast.annotations
  | LCompound compound -> ()

(* Specific to constants for now *)
let analyse_constant_propagation linast =
  Ident.Tbl.clear ident_table;
  analyse_linast linast
