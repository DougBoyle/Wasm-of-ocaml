open Typedtree
open Linast
open Types
open CompileMatch
open LineariseHelp
open LinastUtils

(* May end up needing different functions for translating to linast vs compound vs imm expr *)
(* Can encode quite nicely as a value of required type accompanied with a set of needed linast bindings (to do left-to-right) *)

(* A list equivalent to a Linast: Collect a sequence of the bindings needed as we translate things
   then merge them into a Linast tree at the end. *)

let unify_constructor_tag = function
  | Cstr_constant i -> Int32.shift_left (Int32.of_int i) 1
  | Cstr_block i -> Int32.logor (Int32.shift_left (Int32.of_int i) 1) 1l
  | _ -> raise NotSupported

let translate_ident path = function
  | Val_prim p -> Primitives.translate_prim p
  (* May need to handle some identifiers which point directly to runtime functions e.g. Stdlib!.min
     Could also just 'hack' these into the file as usual idents but would be inefficient and probably slower. *)
  | _ -> (match path with Path.Pident id -> id | _ -> raise NotImplemented)


(* Keep refering to translCore to catch special cases of labelled args, primitives, etc. *)
let rec translate_imm ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) =
  match exp_desc with
  |  Texp_ident (path, idLoc, valDesc) -> (Imm.id (translate_ident path valDesc.val_kind), [])

  | Texp_constant c -> (Imm.const c, [])

  (* Can probably rewrite as a fold *)
  | Texp_let(Nonrecursive, [], e) -> translate_imm e
  | Texp_let(Nonrecursive, {vb_pat;vb_expr}::rest, body) ->
      let (exp, exp_setup) = translate_compound vb_expr in
      let bindList = getBindings vb_pat exp in
      let bind_setup = List.map (fun (id, e) -> BLet(id, e)) bindList in
      let (rest, rest_setup) = translate_imm ({e with exp_desc=Texp_let(Nonrecursive, rest, body)}) in
      (rest, exp_setup @ bind_setup @ rest_setup)

  | Texp_let(Recursive, binds, body) ->
      let (binds, binds_setup) = List.split (List.map (fun {vb_pat; vb_expr} -> (vb_pat, translate_compound vb_expr)) binds) in
      let (required_binds, required_binds_setup) = List.split binds_setup in
      let names = List.map (function
          | {pat_desc=Tpat_var(id, _)} -> id
          | _ -> raise NotSupported (* LHS of let rec must be a function identifier *)) binds in
      let (body, body_setup) = translate_compound body in
      let bodyIdent = Ident.create_local "letrec" in
      (Imm.id bodyIdent, (List.concat required_binds_setup)
                             @ [BLetRec (List.combine names required_binds)]
                             @ body_setup
                             @ [BLet(bodyIdent, body)])


  | Texp_tuple l ->
    let id = Ident.create_local "tuple" in
    let (args, setup) = List.split (List.map translate_imm l) in
    (Imm.id id, (List.concat setup) @ [BLet(id, Compound.makeblock 0l args)])

  | Texp_construct (identLoc, desc, l) ->
    let id = Ident.create_local "block" in
    let (args, setup) = List.split (List.map translate_imm l) in
    (Imm.id id, (List.concat setup) @ [BLet(id, Compound.makeblock (unify_constructor_tag desc.cstr_tag) args)])

 (* Made easier by fact that Typedtree always puts fields in order, regardless of order in program.
    Hence no need to do sorting or check label descriptions *)
 (* TODO: Having to convert arrays to lists suggests I should represent things slightly differently here *)
  | Texp_record {fields; extended_expression} ->
    let id = Ident.create_local "record" in
    (match extended_expression with
    (* Not built off of anything so each field must be Overridden *)
    | None -> let (args, setup) =
        List.split (List.map (function (_, Overridden(_, e)) -> translate_imm e | _ -> raise NotSupported) (Array.to_list fields))
      in (Imm.id id, (List.concat setup) @ [BLet(id, Compound.makeblock 0l args)])
    | Some e -> let extract_field original i = (function
         | (_, Overridden(_, e)) -> translate_imm e
         | (_, Kept _) -> let fieldId = Ident.create_local "field" in
           (Imm.id fieldId, [BLet(id, Compound.field original (Int32.of_int i))]))
       in let (original, original_setup) = translate_imm e in
       let (args, setup) = List.split (List.mapi (extract_field original) (Array.to_list fields))
       in (Imm.id id, original_setup @ (List.concat setup) @ [BLet(id, Compound.makeblock 0l args)])

    )

  | Texp_field (e, identLoc, labelDesc) ->
    let id = Ident.create_local "field" in
    let (record, setup) = translate_imm e in
    (Imm.id id, setup @ [BLet(id, Compound.field record (Int32.of_int labelDesc.lbl_pos))])

  | Texp_setfield (e, identLoc, labelDesc, v) ->
    let id = Ident.create_local "setfield" in
    let (record, record_setup) = translate_imm e in
    let (value, value_setup) = translate_imm v in
    (Imm.id id, record_setup @ value_setup @ [BLet(id, Compound.setfield record (Int32.of_int labelDesc.lbl_pos) value)])

  | Texp_ifthenelse (e1, e2, e3opt) ->
    let (e3_lin, e3_setup) = (match e3opt with Some e -> translate_linast e
      (* TODO: Use a single global unit, or treat as int not block *)
      | None -> let unitId = Ident.create_local "unit" in
                (LinastExpr.compound (Compound.imm (Imm.id unitId)), [BLet(unitId, Compound.makeblock 0l [])])) in
    let (e1_imm, e1_setup) = translate_imm e1 in
    let (e2_lin, e2_setup) = translate_linast e2 in
    let id = Ident.create_local "ifthenelse" in
    (Imm.id id, e1_setup @ e2_setup @ e3_setup @ [BLet(id, Compound.mkif e1_imm e2_lin e3_lin)])

(*

  | Texp_function of { arg_label : arg_label; param : Ident.t;
      cases : value case list; partial : partial; }
        (** [Pexp_fun] and [Pexp_function] both translate to [Texp_function].
            See {!Parsetree} for more details.

            [param] is the identifier that is to be used to name the
            parameter of the function.

            partial =
              [Partial] if the pattern match is partial
              [Total] otherwise.
         *)
  | Texp_apply of expression * (arg_label * expression option) list
        (** E0 ~l1:E1 ... ~ln:En

            The expression can be None if the expression is abstracted over
            this argument. It currently appears when a label is applied.

            For example:
            let f x ~y = x + y in
            f ~y:3

            The resulting typedtree for the application is:
            Texp_apply (Texp_ident "f/1037",
                        [(Nolabel, None);
                         (Labelled "y", Some (Texp_constant Const_int 3))
                        ])
         *)
  | Texp_match of expression * computation case list * partial
        (** match E0 with
            | P1 -> E1
            | P2 | exception P3 -> E2
            | exception P4 -> E3

            [Texp_match (E0, [(P1, E1); (P2 | exception P3, E2);
                              (exception P4, E3)], _)]
         *)


  | Texp_variant of label * expression option
  | Texp_record of {
      fields : ( Types.label_description * record_label_definition ) array;
      representation : Types.record_representation;
      extended_expression : expression option;
    }
        (** { l1=P1; ...; ln=Pn }           (extended_expression = None)
            { E0 with l1=P1; ...; ln=Pn }   (extended_expression = Some E0)

            Invariant: n > 0

            If the type is { l1: t1; l2: t2 }, the expression
            { E0 with t2=P2 } is represented as
            Texp_record
              { fields = [| l1, Kept t1; l2 Override P2 |]; representation;
                extended_expression = Some E0 }
        *)
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression

  | Texp_assert of expression
  | Texp_pack of module_expr
  | Texp_letop of {
      let_ : binding_op;
      ands : binding_op list;
      param : Ident.t;
      body : value case;
      partial : partial;
    }
  | Texp_unreachable
  | Texp_extension_constructor of Longident.t loc * Path.t
  *)
  | _ -> raise NotSupported

and translate_compound ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) = raise NotImplemented

and translate_linast ({exp_desc;exp_loc;exp_extra;exp_type;exp_env;exp_attributes} as e) = raise NotImplemented

let translate_binding {vb_pat; vb_expr; vb_attributes; vb_loc} = raise NotImplemented

let translate_structure_item {str_desc; str_loc; str_env} =
  match str_desc with
  | Tstr_eval (e, _) -> raise NotImplemented
  | Tstr_value (Nonrecursive, bindList) -> raise NotImplemented
  | Tstr_value (Recursive, bindList) -> raise NotImplemented
  | Tstr_type _ | Tstr_typext _ | Tstr_attribute _ -> raise NotImplemented (* Should be ignored *)
  | _ -> raise NotSupported

let rec translate_structure {str_items; str_type; str_final_env} = raise NotImplemented