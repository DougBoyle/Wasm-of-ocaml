(* Propagate knowledge of tag values up to branches/function returns.
   Allows removing trivial match statements e.g.
     match Cons(x,y) with Cons(_,_) -> ... | Nil -> ...
   Can clearly replace with just the first case. *)
open Linast

(* Assume working with just one annotation at a time *)
let ident_table = (Ident.Tbl.create 50 : annotation Ident.Tbl.t)

(* Always replaces any previous annotation. Wasteful but likely ensures correct.
   Reference updated so anything storing a pointer to the same place is also updated. *)
let add_tag_annotation annot annotations =
  annotations := annot::(List.filter (function Tag _ -> false | _ -> true) (!annotations))

let find_annot_opt annotations =
  List.find_opt (function Tag _ -> true | _ -> false) (!annotations)

(* Updates to_annots with the relevant one from from_annots if it is present *)
let copy_annotation from_annots to_annots =
  match find_annot_opt from_annots with None -> ()
    | Some annot -> add_tag_annotation annot to_annots

(* Tags are just ints, so very easy to check *)
let rec copy_if_on_all from_list to_annots = match from_list with
  | [] -> failwith "Given an empty list"
  | [Some annot] -> add_tag_annotation annot to_annots
  | (Some annot1)::(((Some annot2)::_) as rest) -> if annot1 = annot2 then copy_if_on_all rest to_annots else ()
  | (Some _)::None::_ | None :: _ -> ()

(* Updates the annotations on imm *)
let analyse_imm (imm : imm_expr) = match imm.desc with
  | ImmIdent id -> (match Ident.Tbl.find_opt ident_table id with
    (* Update the annotations on that ImmIdent with the tracked annotation *)
    | Some annot -> add_tag_annotation annot imm.annotations
    | None -> ())
  | _ -> ()

(* *)
let rec analyse_compound (compound : compound_expr) = match compound.desc with
  (* Copy up annotations *)
  | CImm imm ->
    analyse_imm imm;
    copy_annotation imm.annotations compound.annotations;

  (*** The only part thats actually significant ***)
  (* Do actual changing of tree structure in optConstants so that this can just be a side-effect function *)
  (* Separates analysis from optimisations *)
  | CMakeBlock (i, _) ->
    add_tag_annotation (Tag i) compound.annotations

  (* Take the intersection of the annotation on each linast expression *)
  | CIf (_, body1, body2) ->
    analyse_linast body1;
    analyse_linast body2;
    copy_if_on_all [find_annot_opt body1.annotations; find_annot_opt body2.annotations] compound.annotations

  (* result is always unit so, for constants optimisations, can't do anything *)
  | CWhile (cond, body) ->
    analyse_linast cond;
    analyse_linast body
  | CFor (_, _, _, _, body) | CFunction (_, body) ->
    analyse_linast body

  | CSwitch (_, cases, default) ->
    List.iter (fun (_, body) -> analyse_linast body) cases;
    (match default with Some body -> analyse_linast body | None -> ());

    copy_if_on_all (List.map (fun (_, expr) -> find_annot_opt expr.annotations)
      (match default with Some body -> (-1, body)::cases | None -> cases)) compound.annotations

  | CMatchTry (_, body, handle) ->
    analyse_linast handle;
    analyse_linast body;
    (* Unclear if anything more can be assumed about result without greater analysis *)
    copy_if_on_all [find_annot_opt body.annotations; find_annot_opt handle.annotations] compound.annotations

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
let analyse_tags linast =
  Ident.Tbl.clear ident_table;
  analyse_linast linast
