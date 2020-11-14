open Typedtree
open Linast
open Types
open CompileMatch
open LineariseHelp
open LinastUtils


(* May end up needing different functions for translating to linast vs compound vs imm expr *)
(* Can encode quite nicely as a value of required type accompanied with a set of needed linast bindings *)

(* A list equivalent to a Linast: Collect a sequence of the bindings needed as we translate things
   then merge them into a Linast tree at the end. *)

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

  | Texp_let(Nonrecursive, [], e) -> translate_imm e
  | Texp_let(Nonrecursive, {vb_pat;vb_expr}::rest, body) ->
      let (exp, exp_setup) = translate_compound vb_expr in
      let bindList = getBindings vb_pat exp in
      let bind_setup = List.map (fun (id, e) -> BLet(id, e)) bindList in
      let (rest, rest_setup) = translate_imm ({e with exp_desc=Texp_let(Nonrecursive, rest, body)}) in
      (rest, exp_setup @ bind_setup @ rest_setup)
(*
  | Texp_let(Recursive, bindingList, e) -> (* TODO: Implement the recursive version *)

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
  | Texp_tuple of expression list
        (** (E1, ..., EN) *)
  | Texp_construct of
      Longident.t loc * Types.constructor_description * expression list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
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
  | Texp_field of expression * Longident.t loc * Types.label_description
  | Texp_setfield of
      expression * Longident.t loc * Types.label_description * expression
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

let translate_binding {vb_pat; vb_expr; vb_attributes; vb_loc} = raise NotImplemented

let translate_structure_item {str_desc; str_loc; str_env} =
  match str_desc with
  | Tstr_eval (e, _) -> raise NotImplemented
  | Tstr_value (Nonrecursive, bindList) -> raise NotImplemented
  | Tstr_value (Recursive, bindList) -> raise NotImplemented
  | Tstr_type _ | Tstr_typext _ | Tstr_attribute _ -> raise NotImplemented (* Should be ignored *)
  | _ -> raise NotSupported

let rec translate_structure {str_items; str_type; str_final_env} = raise NotImplemented