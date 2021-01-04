open Linast
open Types

let defaultLoc = Location.none
let defaultEnv = Env.empty

exception NotImplemented of string
exception NotSupported (* TODO: Modify earlier parts of OCaml frontend to not accept these elements *)

(* Don't worry about passing around locations/environments for now, just a hastle *)
module Imm = struct
  let mk annotations d : imm_expr =
      {desc=d;
       loc=defaultLoc;
       env=defaultEnv;
       annotations}
  let id ?(annotations=ref []) id = mk annotations (ImmIdent id)
  let const ?(annotations=ref []) const = mk annotations (ImmConst const)
end

module Compound = struct
  let mk annotations d : compound_expr =
    {desc=d;
     loc=defaultLoc;
     env=defaultEnv;
     annotations}
  let imm ?(annotations=ref []) imm = mk annotations (CImm imm)
  let fail ?(annotations=ref []) i = mk annotations (CMatchFail i)
  let unary ?(annotations=ref []) op imm = mk annotations (CUnary (op, imm))
  let binary ?(annotations=ref []) op imm1 imm2 = mk annotations (CBinary (op, imm1, imm2))
  let setfield ?(annotations=ref []) imm1 i imm2 = mk annotations (CSetField (imm1, i, imm2))
  let field ?(annotations=ref []) imm i = mk annotations (CField (imm, i))
  let arrayset ?(annotations=ref []) imm1 imm2 imm3 = mk annotations (CArraySet (imm1, imm2, imm3))
  let arrayget ?(annotations=ref []) imm1 imm2 = mk annotations (CArrayGet (imm1, imm2))
  let makeblock ?(annotations=ref []) i args = mk annotations (CMakeBlock (i, args))
  let gettag ?(annotations=ref []) imm = mk annotations (CGetTag imm)
  let mkif ?(annotations=ref []) imm e1 e2 = mk annotations (CIf (imm, e1, e2))
  let mkwhile ?(annotations=ref []) imm e = mk annotations (CWhile (imm, e))
  let mkfor ?(annotations=ref []) id start stop dir e = mk annotations (CFor (id, start, stop, dir, e))
  let mkswitch ?(annotations=ref []) imm cases partial = mk annotations (CSwitch (imm, cases, partial))
  let matchtry ?(annotations=ref []) i e1 e2 = mk annotations (CMatchTry (i, e1, e2))
  let app ?(annotations=ref []) imm args = mk annotations (CApp (imm, args))
  let mkfun ?(annotations=ref []) params e = mk annotations (CFunction (params, e))
end

module LinastExpr = struct
  let mk annotations d : linast_expr =
      {desc=d;
       loc=defaultLoc;
       env=defaultEnv;
       annotations}
  let mklet ?(annotations=ref []) id export e body = mk annotations (LLet (id, export, e, body))
  let mkletrec ?(annotations=ref []) binds body = mk annotations (LLetRec (binds, body))
  let seq ?(annotations=ref []) e1 e2 = mk annotations (LSeq (e1, e2))
  let compound ?(annotations=ref []) e = mk annotations (LCompound e)
end

(* Now that constant only constructors are represented as integers, unit = 0l *)
let unit_value = Imm.const (Asttypes.Const_int 0)

let get_const_constructor_tag = function
  | Cstr_constant i -> i
  | _ -> failwith "Not a constant constructor"

(* Assign constant/block constructor tags to disjoint, contiguous values *)
let unify_constructor_tag desc = match desc.cstr_tag with
  | Cstr_constant i -> i
  | Cstr_block i -> i + desc.cstr_consts
  | _ -> raise NotSupported

type linast_setup =
  | BEffect of compound_expr
  | BLet of Ident.t * compound_expr
  | BLetRec of (Ident.t * compound_expr) list

let get_global_flag exported id = if List.exists (fun x -> x = id) exported then Export else Local

let rec binds_to_anf ?(exported=[]) binds body =
  List.fold_right (fun bind body ->
     match bind with
     | BEffect comp -> LinastExpr.seq comp body
     | BLet(id, comp) -> LinastExpr.mklet id (get_global_flag exported id) comp body
     | BLetRec lets -> LinastExpr.mkletrec (List.map (fun (id, e) -> (id, get_global_flag exported id, e)) lets) body
   ) binds body

(* Primative name (string) -> Ident *)
let primIds : (string * Ident.t) list ref = ref []
(* List of Binds to include in program *)
let primBinds : linast_setup list ref = ref []

(* 10 was arbitrarily chosen as initial size *)
let handler_envs : (int32, Ident.Set.t) Hashtbl.t = Hashtbl.create 10
let update_handler_env i env =
  match Hashtbl.find_opt handler_envs i with
  | Some e -> Hashtbl.replace handler_envs i (Ident.Set.union e env)
  | None -> Hashtbl.add handler_envs i env
(* Getting handler env removes it from the hashtbl, so never need an explicit reset between uses.
   Never actually an issue as handler identifiers aren't reused, so compiler only does this once per handler. *)
let get_handler_env i env = match Hashtbl.find_opt handler_envs i with
  | Some e -> Hashtbl.remove handler_envs i; Ident.Set.union env e
  | None -> env

(* Env is variables bound in body of expression *)
let rec free_vars env (e : linast_expr) =
  match e.desc with
  | LSeq(fst, rest) ->
    Ident.Set.union (free_vars_comp env fst) (free_vars env rest)
  | LCompound c -> free_vars_comp env c
  | LLet(id, _, bind, body) -> Ident.Set.union (free_vars_comp env bind) (free_vars (Ident.Set.add id env) body)
  | LLetRec(binds, body) ->
    let with_names = List.fold_left (fun acc (id, _, _) -> Ident.Set.add id acc) env binds in
    let free_binds = List.fold_left
     (fun acc (_, _, body) -> Ident.Set.union acc (free_vars_comp with_names body)) Ident.Set.empty binds in
    Ident.Set.union free_binds (free_vars with_names body)

and free_vars_comp env (c : compound_expr) = match c.desc with
  | CFunction(args, body) -> free_vars (Ident.Set.union env (Ident.Set.of_list args)) body
  | CIf(cond, thn, els) ->
    Ident.Set.union (free_vars_imm env cond) (Ident.Set.union (free_vars env thn) (free_vars env els))
  | CFor(id, arg1, arg2, _, body) ->
    Ident.Set.union (free_vars (Ident.Set.add id env) body)
    (Ident.Set.union (free_vars_imm env arg1) (free_vars_imm env arg2))
  | CWhile(cond, body) -> Ident.Set.union (free_vars env cond) (free_vars env body)
  | CSwitch(arg, branches, default) ->
    let branch_vars = List.fold_left (fun acc (_, b) -> Ident.Set.union (free_vars env b) acc)
      (free_vars_imm env arg)
      branches in
    (match default with None -> branch_vars | Some b -> Ident.Set.union (free_vars env b) branch_vars)
  | CUnary(_, arg) | CField(arg, _) | CGetTag(arg) -> free_vars_imm env arg
  | CBinary(_, arg1, arg2) | CSetField(arg1, _, arg2) | CArrayGet(arg1, arg2) ->
    Ident.Set.union (free_vars_imm env arg1) (free_vars_imm env arg2)
  | CArraySet(arg1, arg2, arg3) ->
    Ident.Set.union (Ident.Set.union (free_vars_imm env arg1) (free_vars_imm env arg2)) (free_vars_imm env arg3)
  | CApp(fn, args) ->
    List.fold_left (fun acc a -> Ident.Set.union (free_vars_imm env a) acc) (free_vars_imm env fn) args
  | CMakeBlock(_, args) ->
    List.fold_left (fun acc a -> Ident.Set.union (free_vars_imm env a) acc) Ident.Set.empty args
  | CImm i -> free_vars_imm env i
    (* Fail cases record the env at that point, union taken to avoid counting varaibles in handler which
       are bound by matching in the try block. Doesn't discount actual free varaibles due to use of unique Idents.
       Not actually necessary until pattern matching optimisations implemented *)
  | CMatchFail i -> update_handler_env i env; Ident.Set.empty
  | CMatchTry (i, body, handler) ->
    (* Must be evaluated in this order *)
    let free_in_body = free_vars env body in
    Ident.Set.union free_in_body (free_vars (get_handler_env i env) handler)


and free_vars_imm env (i : imm_expr) = match i.desc with
  | ImmIdent x when not (Ident.Set.mem x env) -> Ident.Set.singleton x
  | _ -> Ident.Set.empty

(* Just needs the maximum since same 'env' (contains most recent stack index) is passed to compilation
   of both sides e.g. Seq(hd, tl). tl doesn't use anything added to scope in hd so can just overwrite.
   Makes semantics of the handle case difficult, need to know how variables have previously been bound
   when going to compile handle case, likely by using references as a 'cheat' to scoping in this case.  *)
(* Like a very basic form of register colouring! *)
(* Assumption that all vars can use the same type of local variable - is this true/relevant? *)
let rec count_vars (ast : linast_expr) = match ast.desc with
  (* Split up how many things needed at a time.
     For let x = e in e', can overwrite all variables in e once its constructed, then just have x and e' *)
  | LLet (_, _, comp, body) -> max (count_vars_comp comp) (1 + count_vars body)
  | LLetRec (binds, body) ->
    let max_binds = List.fold_left max 0 (List.map (fun (_, _, c) -> count_vars_comp c) binds) in
    max max_binds (List.length binds + (count_vars body))
   | LSeq(comp, linast) -> max (count_vars_comp comp) (count_vars linast)
   | LCompound c -> count_vars_comp c

and count_vars_comp c =
  match c.desc with
  | CIf(_, t, f) -> max (count_vars t) (count_vars f)
  | CFor(_, _, _, _, body) -> (count_vars body) + 2 (* 1 for each of start/end value *)
  | CWhile(cond, body) -> (count_vars cond) + (count_vars body)
  | CSwitch(_, cases, default) ->
   let max_case = List.fold_left max 0 (List.map (fun (_, b) -> count_vars b) cases) in
   (match default with None -> max_case | Some c -> max max_case (count_vars c))
  | CMatchTry (_, e1, e2) -> (count_vars e1) + (count_vars e2)
  | _ -> 0
