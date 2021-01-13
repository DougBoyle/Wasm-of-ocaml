(*
  Propagate known values for tags/fields of blocks in memory. Very restricted form of CFA-0.
  Annotations are used by OptConstants to replace 'GetField' or 'GetTag' with their known values.
  If block is not used anywhere else, dead assignment elimination will avoid the memory allocation entirely.
  TODO: Worth implementing greater form of CFA-0? Would mean needing another graph representation? Do lower down?
*)
open Linast

let known_tags = (Ident.Tbl.create 50 : int Ident.Tbl.t)
let known_fields = (Ident.Tbl.create 50 : (imm_expr option) list Ident.Tbl.t)

let add_annotation annot annotations =
  annotations := annot::(!annotations)
let add_annotations annot_list annotations =
  List.iter (fun annot -> add_annotation annot annotations) annot_list

(* Updates the annotations on imm *)
let analyse_imm (imm : imm_expr) = match imm.desc with
  | ImmIdent id -> (match Ident.Tbl.find_opt known_tags id with
    (* Update the annotations on that ImmIdent with the tracked annotation *)
    | Some i -> add_annotation (Tag i) imm.annotations
    | None -> ());
    (match Ident.Tbl.find_opt known_fields id with
    (* Update the annotations on that ImmIdent with the tracked annotation *)
    | Some fields -> add_annotation (FieldImms fields) imm.annotations
    | None -> ())
  | _ -> ()

let find_tag_opt annotations =
  List.find_opt (function Tag _ -> true | _ -> false) (!annotations)
let find_fields_opt annotations =
  List.find_opt (function FieldImms _ -> true | _ -> false) (!annotations)

let copy_annotations from_annots to_annots =
  (match find_tag_opt from_annots with None -> ()
     | Some annot -> add_annotation annot to_annots);
  (match find_fields_opt from_annots with None -> ()
     | Some annot -> add_annotation annot to_annots)

(* Given that result could be one of two values, take the intersection of knowledge about fields *)
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

let merge_tags = function
  | (Some (Tag i), Some (Tag j)) when i = j -> Some (Tag i)
  | _ -> None

(* For If statements and Switch cases, take the intersection of information known about each case *)
let merge_annotations = function
  | annotations::others ->
    List.fold_right (fun annotations intersection ->
      (match (merge_tags (find_tag_opt annotations,
        List.find_opt (function Tag _ -> true | _ -> false) intersection))
      with None -> [] | Some annot -> [annot]) @
      (match (merge_fieldimms (find_fields_opt annotations,
        List.find_opt (function FieldImms _ -> true | _ -> false) intersection))
      with None -> [] | Some annot -> [annot])
    ) others (!annotations)
  | _ -> failwith "No elements given to merge"

let rec analyse_compound (compound : compound_expr) = match compound.desc with
  (* Copy up annotations *)
  | CImm imm ->
    analyse_imm imm;
    copy_annotations imm.annotations compound.annotations;

  | CMakeBlock (i, imms) ->
    List.iter analyse_imm imms;
    add_annotation (Tag i) compound.annotations;
    (* Can only keep analysis about fields known to be immutable *)
    List.iter (function ImmutableBlock l ->
      let imm_list = List.mapi (fun idx (imm : imm_expr) -> if List.nth l idx then Some imm else None) imms in
      if List.exists (function Some _ -> true | _ -> false) imm_list
        then add_annotation (FieldImms imm_list) compound.annotations | _ -> ()) (!(compound.annotations))

  (* Take the intersection of the annotation on each linast expression *)
  | CIf (_, body1, body2) | CMatchTry (_, body1, body2)  ->
    analyse_linast body1;
    analyse_linast body2;
    add_annotations (merge_annotations [body1.annotations; body2.annotations]) compound.annotations

  (* result is always unit so, for constants optimisations, can't do anything *)
  | CWhile (cond, body) ->
    analyse_linast cond;
    analyse_linast body
  | CFor (_, _, _, _, body)| CFunction(_, body) ->
    analyse_linast body

  | CSwitch (_, cases, default) ->
    List.iter (fun (_, body) -> analyse_linast body) cases;
    (match default with Some body -> analyse_linast body | None -> ());

    let bodies = (match default with None -> [] | Some body -> [body]) @ (List.map snd cases) in
    add_annotations (merge_annotations (List.map (fun body -> body.annotations) bodies)) compound.annotations

  | _ -> ()

and analyse_linast linast = match linast.desc with
  (* Add analysis of binds to ident_table. Set overall linast analysis to combination of bind/body *)
  | LLet(id, mut, bind, body) ->
    analyse_compound bind;
    (if mut <> Mut then
     (match find_tag_opt bind.annotations with
       | Some (Tag i) -> Ident.Tbl.add known_tags id i
       | _ -> ());
     (match find_fields_opt bind.annotations with
       | Some (FieldImms imms) -> Ident.Tbl.add known_fields id imms
       | _ -> ()));
    analyse_linast body;
    copy_annotations body.annotations linast.annotations

  | LLetRec(_, body) ->
    (* Each bind is a function so, for analysis of compounds, no use processing *)
    analyse_linast body;
    copy_annotations body.annotations linast.annotations
  (* compound result gets ignored so no point evaluating *)
  | LSeq (compound, body) ->
    analyse_linast body;
    copy_annotations body.annotations linast.annotations
  | LCompound compound -> ()

(* Specific to constants for now *)
let analyse linast =
  Ident.Tbl.clear known_tags;
  Ident.Tbl.clear known_fields;
  analyse_linast linast
