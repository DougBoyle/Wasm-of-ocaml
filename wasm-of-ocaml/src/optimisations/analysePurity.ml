(* Add analysis about which terms are pure or not and propagate it through
   bindings to idents/use in larger compounds. Allows smarter purity analysis.
   Let id = {some compound} in ... {Imm.id id; ptr to same analysis as on compound}.
   Annotations is mutable reference to allow updating to share list without returning new term. *)
open Linast

(* Pointer to the actual annotations on compounds idents are bound to, must be copied not shared *)
let ident_analysis = (Ident.Tbl.create 50 : annotations Ident.Tbl.t)

(* Mark element as not causing any side effects when evaluated. Does not guarentee that result
   is the same as any previous time it was evaluated (e.g. getField on a mutable field) *)
let add_annotation annot annotations = if not (List.mem annot (!annotations)) then
  annotations := annot::(!annotations)

let copy_immutable_annotation imms annotations =
  if List.for_all (fun imm -> List.mem Immutable (!(imm.annotations))) imms then
    annotations := Immutable::(!annotations)

(* Updates the annotations on imm *)
let analyse_imm imm = match imm.i_desc with
  (* Assumes imm doesn't already have any annotations on it *)
  | ImmIdent id -> (match Ident.Tbl.find_opt ident_analysis id with
    (* Must copy original annotations in the list of annotations gets modified *)
    | Some annotations -> imm.i_annotations <- annotations
    (* Unseen ident, possibly a function argument (nothing assumed, would need to anlayse all calls) *)
    | None -> ())
  | ImmConst _ ->  add_annotation Immutable imm.i_annotations;
     add_annotation Pure imm.i_annotations

let rec analyse_compound handlers compound = match compound.c_desc with
  (* Copy up annotations *)
  | CImm imm ->
    analyse_imm imm;
    (* Pure/Immutable added after so need to take a copy *)
    compound.c_annotations <- ref (!(imm.i_annotations));
    add_annotation Immutable compound.c_annotations;
    add_annotation Pure compound.c_annotations
  | CMatchFail (-1l) -> () (* Could mark expression as failing - no different to just constant folding the trap *)
  | CMatchFail i -> compound.c_annotations <- List.assoc i handlers (* annotation of the handler it jumps to *)
  (* Currently all unary/binary operations are pure/immutable result *)
  | CUnary (_, imm) ->
    add_annotation Immutable compound.c_annotations;
    add_annotation Pure compound.c_annotations
  | CBinary (_, imm1, imm2) ->
    add_annotation Immutable compound.c_annotations;
    add_annotation Pure compound.c_annotations
  (* TODO: Without more complex analysis of existing values (graph algorithm), can't infer about setfield *)
  | CSetField _ -> ()
  | CField (imm, idx) ->
    analyse_imm imm;
    List.iter
    (* Extract out any annotations set on that field *)
    (* The ith element of Fields only contains annotations if that field of the block is immutable,
       see MakeBlock case below. In this case, value cannot have been overwritten so must be the same,
       with the same properties e.g. being immutable, pure, etc. since getting the field doesn't change that. *)
    (* TODO: Make use of fact that nth failing => case can never occur. Needs a Impossible annotation *)
    (function Fields l -> (match List.nth_opt l idx with
           | Some annotations -> compound.c_annotations <- annotations
           | None -> ())
      | _ -> ()) (!(imm.i_annotations));
    add_annotation Pure compound.c_annotations
  | CArraySet _ -> ()
  | CArrayGet _ -> add_annotation Pure compound.c_annotations (* Mutable fields so can't guarantee anything else *)
  (* Must not set annotations on mutable fields, could be set to another ident in future *)
  (* can mark as immutable if only the immutable fields of it are ever used *)
  | CMakeBlock (_, imms) ->
    List.iter analyse_imm imms;
    List.iter
    (function ImmutableBlock l ->
      (* For fixed fields, copy across any annotations from Imms *)
      add_annotation (Fields (List.mapi (fun idx imm -> if List.nth l idx
       then imm.i_annotations else ref []) imms)) compound.c_annotations;
      (* If every field is fixed, block can be viewed as immutable *)
      if List.for_all (fun b -> b) l then add_annotation Immutable compound.c_annotations | _ -> ())
      (!(compound.c_annotations));
    (* Same to eliminate a dead assignment to MakeBlock *)
    add_annotation Pure compound.c_annotations
  | CGetTag _ -> add_annotation Immutable compound.c_annotations; add_annotation Pure compound.c_annotations

  (* Safe to take the intersection of the annotations of each linast expression *)
  | CIf (_, body1, body2) ->
    analyse_linast handlers body1;
    analyse_linast handlers body2;
    (* TODO: intersection, may want to do recursively for Field annotation
             e.g. true -> [Pure, _] and false -> [Pure, _, _] could preserved field0 = Pure *)
    List.iter (fun annot -> if List.mem annot (!(body2.annotations)) then
      add_annotation annot compound.c_annotations) (!(body1.annotations))
  | CWhile (cond, body) ->
    analyse_linast handlers cond;
    analyse_linast handlers body;
    List.iter (fun annot -> if List.mem annot (!(body.annotations)) then
      add_annotation annot compound.c_annotations) (!(cond.annotations));
    if List.mem Pure (!(compound.c_annotations)) then add_annotation Immutable compound.c_annotations

  | CFor (_, _, _, _, body) ->
    analyse_linast handlers body;
    (* May add an Immutable annotation after so need to copy *)
    compound.c_annotations <- ref (!(body.annotations));
    if List.mem Pure (!(compound.c_annotations)) then add_annotation Immutable compound.c_annotations

  (* Intersection of each possible result *)
  | CSwitch (_, cases, default) ->
    List.iter (fun (_, body) -> analyse_linast handlers body) cases;
    (match default with Some body -> analyse_linast handlers body | None -> ());
    let body, cases = (match default with Some body -> body, cases
      | None -> match cases with (_, body)::cases -> body, cases | _ -> failwith "Empty match") in
    compound.c_annotations <- ref (List.fold_left (fun annots (_, body) ->
      List.filter (fun annot -> List.mem annot annots) (!(body.annotations))) (!(body.annotations)) cases)

  | CMatchTry (i, body, handle) ->
    analyse_linast handlers handle;
    analyse_linast ((i, handle.annotations)::handlers) body;
    compound.c_annotations <- body.annotations

  (* Can't analyse function body in terms of arguments, so its analysis assumes all arguemnts are mutable
     etc. hence no point in analysing here. *)
  (* Any properties guarenteed about function body are also guarenteed about its application.
     Need to unroll currying in analysis *)
  (* Due to how analysis propagates, side effect within a function will prevent analysis of a pure function
     return value from propagating out of the function if first one is over-applied *)
  | CApp (f, args) ->
    analyse_imm f;
    compound.c_annotations <- List.fold_left
     (fun func_annots arg ->
       let latent_analysis =
         List.fold_left (fun annots -> (function Latent body -> body | _ -> annots)) (ref []) (!func_annots) in
       ref (List.filter (function ((Pure|Immutable) as annot) -> List.mem annot (!func_annots) | _ -> true)
           (!latent_analysis)))
     f.i_annotations args

  (* Not performing CFA, so can't infer latent effects of function arguments, will only be marked as pure/immutable *)
  (* Creating a closure is always pure/immutable so pass body as Latent annotation *)
  | CFunction (args, body) ->
    analyse_linast handlers body;
    (* Each partial function is pure/immutable *)
    let function_annotations =
      (List.fold_left (fun annots _ -> ref [Pure; Immutable; Latent annots]) body.annotations args) in
    (* Need to remember if function has been tail call optimised/expects args as a tuple *)
    if List.mem TailCallOptimised (!(compound.c_annotations))
      then function_annotations := TailCallOptimised :: (!function_annotations);
    if List.mem Tupled (!(compound.c_annotations))
      then function_annotations := Tupled :: (!function_annotations);
    compound.c_annotations <- function_annotations
  | CAssign _ -> ()

(* Annotations of a linast point to the annotations of the corresponding subterm (compound/smaller linast)
   so must be copied, not shared, during analysis of compounds *)
and analyse_linast handlers linast = match linast.desc with
  (* Add analysis of binds to ident_analysis. Set overall linast analysis to combination of bind/body *)
  | LLet(id, mut, bind, body) ->
    analyse_compound handlers bind;
    (* Only store annalysis for idents that can't change. Mutable assignments introduced by Tail Call Optimisation *)
    if mut <> Mut then Ident.Tbl.add ident_analysis id bind.c_annotations;
    analyse_linast handlers body;
    (* Should copy annotations of body, but Immutable/Pure are affected by evaluating binding. *)
    List.iter (function ((Pure|Immutable) as annot) -> if List.mem annot (!(bind.c_annotations)) then
      add_annotation annot linast.annotations
      | annot -> add_annotation annot linast.annotations)  (!(body.annotations))
  (* function variables not added to the table of Idents, so nothing assumed about the latent effects/termination
     of recursive function. Could extend to find an iterative solution, like strictness analysis *)
  | LLetRec(binds, body) ->
    List.iter (fun (_, _, bind) -> analyse_compound handlers bind) binds;
    List.iter (fun (id, _, bind) -> Ident.Tbl.add ident_analysis id bind.c_annotations) binds;
    analyse_linast handlers body;
    (* know that all recursive bindings are functions so don't need to check binds pure/immutable *)
    linast.annotations <- body.annotations
  (* Copy annotations of body, only pure/immutable if compound is pure *)
  | LSeq (compound, body) ->
    analyse_compound handlers compound;
    analyse_linast handlers body;
    List.iter (function ((Pure|Immutable) as annot) -> if List.mem Pure (!(compound.c_annotations)) then
     add_annotation annot linast.annotations
     | annot -> add_annotation annot linast.annotations)  (!(body.annotations))
  | LCompound compound ->
    analyse_compound handlers compound;
    linast.annotations <- compound.c_annotations

let analyse linast =
  Ident.Tbl.clear ident_analysis;
  analyse_linast [] linast
