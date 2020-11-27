open LinastUtils
open Linast
open Typedtree
open Types

(* TODO: May get some issues with Computation vs Value patterns *)

(* Used whenever a match could fail. Default value is -1 for trap, otherwise this is used to access an outer try/catch *)
(* Great simplification of OCaml's approach, which keeps a list of (int * matrix) pairs, allowing the compiler to not
   only escape a failed match, but potentially jump further down the list of cases based on additional information learned
   about the expression during trying this pattern. *)
let fail_trap = -1l

let fail_count = ref fail_trap

let next_fail_count () =
  fail_count := Int32.add 1l (!fail_count);
  !fail_count

(* pattern -> comp_expr -> BLet list *)
(* value of expression needed in case that it first needs binding to something e.g. TPat_var or alias *)
(* For 'let pattern = expr ...' statements *)
(* Inefficient in some places when expr = Compound.Imm (Imm.id x) - creates another identifier bound to the same thing.
   Should be able to remove this when doing common subexpression elimination/dead assignment elimination. *)
let rec getBindings fail (pat : pattern) (expr : compound_expr) = match pat.pat_desc with
  (* TODO: Even if nothing gets bound, need to ensure expression is evaluated exactly once *)
  | Tpat_any -> [BEffect expr]
  (* TODO: Bit of a hack to avoid trying to bind an identifier to itself e.g. when param of function = only pattern
           Could likely remove in a cleanup pass, as it won't be causing any issues, just looks odd *)
  | Tpat_var (x, _) ->
    let default = [BLet(x, expr)] in
    begin match expr.desc with CImm{desc=ImmIdent i} -> if i = x then [] else default | _ -> default end
  | Tpat_alias (p, x, _) -> (BLet(x, expr)) :: getBindings fail p expr
  | Tpat_constant c ->
    let id = Ident.create_local "constant" in
    let test_result = Compound.binary Eq (Imm.const c) (Imm.id id) in
    let testid = Ident.create_local "isequal" in
    [BLet(id, expr); BLet(testid, test_result);
    BEffect(Compound.mkif
      (Imm.id testid)
      (LinastExpr.compound (Compound.makeblock 0l []))   (* Test passed - do nothing TODO: Use a global unit value *)
      (LinastExpr.compound (Compound.imm (Imm.fail fail))))] (* Test failed - trap/next pattern *)
  | Tpat_tuple l ->
    let id = Ident.create_local "tuple" in
    let tuple_imm = Imm.id id in
    let tuple_element_binds index pat =
      let arg_id = Ident.create_local "tuple_arg" in
      let arg_expr = Compound.imm (Imm.id arg_id) in
      let sub_binds = getBindings fail pat arg_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(arg_id, Compound.field tuple_imm (Int32.of_int index)))::sub_binds)
    in let binds = List.concat (List.mapi tuple_element_binds l) in
    (match binds with [] -> [] | _ -> (BLet(id, expr)::binds))

  (* Case when constructor is just treated as an integer *)
  | Tpat_construct (_, desc, []) when desc.cstr_nonconsts = 0 ->
    getBindings fail {pat with pat_desc=Tpat_constant(get_const_constructor_tag desc.cstr_tag)} expr

  | Tpat_construct (_, {cstr_tag}, pl) ->
    let id = Ident.create_local "construct" in
    let block_imm = Imm.id id in

    (* Test tag - fail if not a match *)
    let tag_id = Ident.create_local "construct_tag" in
    let tag_expr = Compound.gettag block_imm in
    (* Note that this puts an int32 constant in the tree, instead of the usual const_int - other integers aren't converted *)
    let test_result = Compound.binary Eq (Imm.const (Const_int32 (unify_constructor_tag cstr_tag))) (Imm.id tag_id) in
    let testid = Ident.create_local "isequal" in

    (* If tag matches - bind subcomponents *)
    let block_element_binds index pat =
      let elem_id = Ident.create_local "element" in
      let elem_expr = Compound.imm (Imm.id elem_id) in
      let sub_binds = getBindings fail pat elem_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(elem_id, Compound.field block_imm (Int32.of_int index)))::sub_binds)
    in let sub_binds = List.concat (List.mapi block_element_binds pl) in

    [BLet(id, expr);
    BLet(tag_id, tag_expr);
    BLet(testid, test_result);
    BEffect(Compound.mkif
      (Imm.id testid)
      (LinastExpr.compound (Compound.makeblock 0l []))   (* Test passed - do nothing TODO: Use a global unit value *)
      (LinastExpr.compound (Compound.imm (Imm.fail fail)))); (* Test failed - trap/next pattern *)
    ]  @ sub_binds

  | Tpat_record (l, _) ->
    let id = Ident.create_local "record" in
    let record_imm = Imm.id id in
    let field_binds index (_, labelDesc, pat) =
      let field_id = Ident.create_local "field" in
      let field_expr = Compound.imm (Imm.id field_id) in
      let sub_binds = getBindings fail pat field_expr in
      (* Only include the binding for the tuple element if it is actually needed to perform other bindings *)
      (match sub_binds with [] -> [] | _ -> (BLet(field_id, Compound.field record_imm (Int32.of_int labelDesc.lbl_pos)))::sub_binds)
    in let binds = List.concat (List.mapi field_binds l) in
    (match binds with [] -> [] | _ -> (BLet(id, expr)::binds))

  (* Unsure about OR case - how does it interact with matching single or multiple components? *)
  (* Is putting expr into an ident necessary? Avoids evaluating a compound twice, but many wasted bindings if nested ORs *)
  | Tpat_or (p1, p2, _) ->
    let id = Ident.create_local "or" in
    let expr' = Compound.imm (Imm.id id) in
    let new_fail = next_fail_count () in
    let first_binds = getBindings new_fail p1 expr' in
    let second_binds = getBindings fail p2 expr' in
    (* TODO: Annoying that we have to put a dummy 'unit' at the end of each case. May need an extra term to avoid this *)
    let compound = Compound.matchtry new_fail
      (binds_to_anf first_binds (LinastExpr.compound (Compound.makeblock 0l []))) (* Have to put dummy unit at end of tree *)
      (binds_to_anf second_binds (LinastExpr.compound (Compound.makeblock 0l [])))
    in [BLet(id, expr); BEffect compound]


  (* Don't expect to need to match Tpat_value or Tpat_lazy -- GADT, they aren't value patterns *)
  | Tpat_array pl -> raise (NotImplemented __LOC__)
  | Tpat_variant (_, _, _) -> raise (NotImplemented __LOC__)
  | _ -> raise NotSupported

let include_guard fail expr = function
  | None -> (expr, [])
  | Some (comp, setup) ->
    let id = Ident.create_local "guard" in
    let id_imm = Imm.id id in
    (Compound.mkif (id_imm) (LinastExpr.compound comp) (LinastExpr.compound (Compound.imm (Imm.fail fail))),
     setup @ [BLet(id, comp)]) (* May as well just pass it in as an immediate? *)


(* Texp_function is value case list,
   Texp_match is computation case list. Likely want some central 'compile' function and two interface functions.
   Also need to handle merging curried texp_functions into one. *)

(* Just do most naive approach initially, optimise/do grouping once I have something that works at least.
   Can decide based on time available/how bad the initial output is. *)

(* Takes a (pattern * compound) list and Compound to match and outputs a compound (+ setup) which does the match + evaluation*)
(* Ocaml doesn't have ML's general `let f [] [] = a | (x::xs) _ = b | ...` syntax, only 1 expr/pattern to check naively *)
(* TODO: Check for exhaustiveness where possible *)
(* TODO: Use Switch whenever multiple constructor patterns found (partial -> result of the matching) *)
(* TODO: Split head off of each pattern rather than working down linearly one-at-a-time *)
(* Is there every actually any `setup` to do for pattern matching? All part of the binds return by getBinds *)
(* THIS SHOULD POSSIBLY RETURN A LINAST_EXPR INSTEAD OF A COMPOUND - MAKES MIDDLE CASE EASIER TOO *)
let rec compile_match partial fail expr = function
  | [] -> (Compound.imm (Imm.fail fail), []) (* All cases exhausted *)
  | [(pat, (e, setup), guard)] when (match partial with Total -> true | _ -> false) ->
    let binds = getBindings fail pat expr in
    let (e', guard_setup) = include_guard fail e guard in
    (e', binds @ guard_setup @ setup) (* Total so no matchtry, hence need to return the setup separately *)
  | (pat, (e, setup), guard)::rest -> let (rest, rest_setup) = compile_match partial fail expr rest in
    let new_fail = next_fail_count () in
    let binds = getBindings new_fail pat expr in
    let (e', guard_setup) = include_guard new_fail e guard in
    (Compound.matchtry
      new_fail
      (binds_to_anf (binds @ guard_setup @ setup) (LinastExpr.compound e'))
      (binds_to_anf rest_setup (LinastExpr.compound rest)),
    [])


