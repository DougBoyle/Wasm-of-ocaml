(* Propagate any analysis such as pure fields/expressions from compound -> ident -> compound using that ident *)
(* Attempt to write more general helper function that propagateAnalysis, for specific forms of analysis
  (some work wasted in checking if just the annotation we care about is present or not) *)
(* For now, specialised to handling propagating Imms through memory *)
(* TODO: Also handle tags *)
(* Everything takes a filter f to identify the correct type of annotation this analysis is interested in.
   Functor may be useful actually (that or return a record of each mapper) *)
open Linast

(* Assume working with just one annotation at a time *)
let ident_table = (Ident.Tbl.create 50 : annotation Ident.Tbl.t)

(* Always replaces any previous annotation. Wasteful but likely ensures correct.
   Reference updated so anything storing a pointer to the same place is also updated. *)
(* Assumes at most 1 of each annotation present in any list *)
let add_annotation filter annot annotations =
  annotations := annot::(List.filter (fun annot -> not (filter annot)) (!annotations))

let add_annotation_opt filter annot annotations = match annot with
  | None -> () | Some annot -> add_annotation filter annot annotations

(* Updates to_annots with the relevant one from from_annots if it is present *)
let copy_annotation filter from_annots to_annots =
  match List.find_opt filter (!from_annots) with None -> ()
    | Some annot -> add_annotation filter annot to_annots

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
let merge_annots filter from1 from2 =
  merge_fieldimms (List.find_opt filter (!from1), List.find_opt filter (!from2))

(* Updates the annotations on imm *)
let analyse_imm filter (imm : imm_expr) = match imm.desc with
  | ImmIdent id -> (match Ident.Tbl.find_opt ident_table id with
    (* Update the annotations on that ImmIdent with the tracked annotation *)
    | Some annot -> add_annotation filter annot imm.annotations
    | None -> ())
  | _ -> ()

(* Doesn't allow for the same level of analysis as propagateAnalysis i.e. tracking handlers too *)
(* Currently very restricted in the type of analysis it allows performing, needs rewriting in future. *)
(* TODO: Just using for constants for now *)
let rec analyse_compound filter (compound : compound_expr) = match compound.desc with
  (* Copy up annotations *)
  | CImm imm ->
    analyse_imm filter imm;
    copy_annotation filter imm.annotations compound.annotations;

  | CMakeBlock (_, imms) ->
    List.iter (analyse_imm filter) imms;
    List.iter (function ImmutableBlock l ->
      let imm_list = List.mapi (fun idx (imm : imm_expr) -> if List.nth l idx then Some imm else None) imms in
      if List.exists (function Some _ -> true | _ -> false) imm_list
        then add_annotation filter (FieldImms imm_list) compound.annotations | _ -> ()) (!(compound.annotations))

  (* Do actual changing of tree structure in optConstants so that this can just be a side-effect function *)
  (* Separates analysis from optimisations *)

  (* Take the intersection of the annotation on each linast expression *)
  | CIf (_, body1, body2) ->
    analyse_linast filter body1;
    analyse_linast filter body2;
    add_annotation_opt filter (merge_annots filter body1.annotations body2.annotations) compound.annotations

  (* result is always unit so, for constants optimisations, can't do anything *)
  | CWhile (cond, body) ->
    analyse_linast filter cond;
    analyse_linast filter body
  | CFor (_, _, _, _, body) ->
    analyse_linast filter body

  | CSwitch (_, cases, default) ->
    List.iter (fun (_, body) -> analyse_linast filter body) cases;
    (match default with Some body -> analyse_linast filter body | None -> ());
    let body, rest = (match default with Some body -> body, cases
      | None -> match cases with (_, body)::cases -> body, cases | _ -> failwith "Empty match") in
    (* TODO: Rewrite merge function to accept list rather than always pairs. Makes this much neater *)
    add_annotation_opt filter
    (List.fold_left (fun opt_annot (_, linast) -> merge_annots filter
      (match opt_annot with None -> ref [] | Some a -> ref [a]) linast.annotations)
      (List.find_opt filter (!(body.annotations))) rest) compound.annotations

  | CMatchTry (_, body, handle) ->
    analyse_linast filter handle;
    analyse_linast filter body;
    (* Unclear if anything more can be assumed about result without greater analysis *)
    add_annotation_opt filter (merge_annots filter body.annotations handle.annotations) compound.annotations

  | CFunction (_, body) ->
    analyse_linast filter body

  | _ -> ()

and analyse_linast filter linast = match linast.desc with
  (* Add analysis of binds to ident_table. Set overall linast analysis to combination of bind/body *)
  | LLet(id, _, bind, body) ->
    analyse_compound filter bind;
    (match List.find_opt filter (!(bind.annotations)) with None -> ()
     | Some annot -> Ident.Tbl.add ident_table id annot);
    analyse_linast filter body;
    copy_annotation filter body.annotations linast.annotations

  | LLetRec(_, body) ->
    (* Each bind is a function so, for analysis of compounds, no use processing *)
    analyse_linast filter body;
    copy_annotation filter body.annotations linast.annotations
  (* compound result gets ignored so no point evaluating *)
  | LSeq (compound, body) ->
    analyse_linast filter body;
    copy_annotation filter body.annotations linast.annotations
  | LCompound compound -> ()

(* Specific to constants for now *)
let analyse_constant_propagation linast =
  Ident.Tbl.clear ident_table;
  analyse_linast (function FieldImms _ -> true | _ -> false) linast
