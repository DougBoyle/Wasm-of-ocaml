open Linast
open Types

let defaultLoc = Location.none
let defaultEnv = Env.empty

exception NotImplemented of string
exception NotSupported (* TODO: Modify earlier parts of OCaml frontend to not accept these elements *)

(* Don't worry about passing around locations/environments for now, just a hastle *)
module Imm = struct
  let mk d : imm_expr =
      {desc=d;
       loc=defaultLoc;
       env=defaultEnv;
       annotations=ref []}
  let id id = mk (ImmIdent id)
  let const const = mk (ImmConst const)
  let fail i = mk (ImmMatchFail i)
end

module Compound = struct
  let mk d : compound_expr =
    {desc=d;
     loc=defaultLoc;
     env=defaultEnv;
     annotations=ref []}
  let imm imm = mk (CImm imm)
  let unary op imm = mk (CUnary (op, imm))
  let binary op imm1 imm2 = mk (CBinary (op, imm1, imm2))
  let setfield imm1 i imm2 = mk (CSetField (imm1, i, imm2))
  let field imm i = mk (CField (imm, i))
  let arrayset imm1 imm2 imm3 = mk (CArraySet (imm1, imm2, imm3))
  let arrayget imm1 imm2 = mk (CArrayGet (imm1, imm2))
  let makeblock i args = mk (CMakeBlock (i, args))
  let gettag imm = mk (CGetTag imm)
  let mkif imm e1 e2 = mk (CIf (imm, e1, e2))
  let mkwhile imm e = mk (CWhile (imm, e))
  let mkfor id start stop dir e = mk (CFor (id, start, stop, dir, e))
  let mkswitch imm cases partial = mk (CSwitch (imm, cases, partial))
  let matchtry i e1 e2 = mk (CMatchTry (i, e1, e2))
  let app imm args = mk (CApp (imm, args))
  let mkfun params e = mk (CFunction (params, e))
end

module LinastExpr = struct
  let mk d : linast_expr =
      {desc=d;
       loc=defaultLoc;
       env=defaultEnv;
       annotations=ref []}
  let mklet id export e body = mk (LLet (id, export, e, body))
  let mkletrec binds body = mk (LLetRec (binds, body))
  let seq e1 e2 = mk (LSeq (e1, e2))
  let compound e = mk (LCompound e)
end


let unify_constructor_tag = function
  | Cstr_constant i -> Int32.shift_left (Int32.of_int i) 1
  | Cstr_block i -> Int32.logor (Int32.shift_left (Int32.of_int i) 1) 1l
  | _ -> raise NotSupported

type linast_setup =
  | BEffect of compound_expr
  | BLet of Ident.t * compound_expr
  | BLetRec of (Ident.t * compound_expr) list

let get_global_flag exported id = if List.exists (fun x -> x = id) exported then Global else Local

let rec binds_to_anf ?(exported=[]) binds body =
  List.fold_right (fun bind body ->
     match bind with
     | BEffect comp -> LinastExpr.seq comp body
     | BLet(id, comp) -> LinastExpr.mklet id (get_global_flag exported id) comp body
     | BLetRec lets -> LinastExpr.mkletrec (List.map (fun (id, e) -> (id, get_global_flag exported id, e)) lets) body
   ) binds body

(* Primative name -> Ident *)
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

(* TODO: Helper functions to enable translating to wasm (and also optimisation passes later) *)
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
  | CFunction(args, body) ->
    free_vars (Ident.Set.union env (Ident.Set.of_list args)) body
  | CIf(cond, thn, els) ->
    Ident.Set.union (free_vars_imm env cond) (Ident.Set.union (free_vars env thn) (free_vars env els))
  | CFor(id, arg1, arg2, _, body) ->
    Ident.Set.union (free_vars (Ident.Set.add id env) body)
    (Ident.Set.union (free_vars_imm env arg1) (free_vars_imm env arg2))
  | CWhile(cond, body) ->
    Ident.Set.union (free_vars_imm env cond) (free_vars env body)
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
       are bound by matching in the try block. Doesn't discount actual free varaibles due to use of unique Idents. *)
  | CMatchTry (i, body, handler) ->
    (* Must be evaluated in this order *)
    let free_in_body = free_vars env body in
    Ident.Set.union free_in_body (free_vars (get_handler_env i env) handler)


and free_vars_imm env (i : imm_expr) = match i.desc with
  | ImmIdent x when not (Ident.Set.mem x env) -> Ident.Set.singleton x
  (* Track possibly bound variables for when handler body entered *)
  | ImmMatchFail i -> update_handler_env i env; Ident.Set.empty
  | _ -> Ident.Set.empty

(* Just needs the maximum since same 'env' (contains most recent stack index) is passed to compilation
   of both sides e.g. Seq(hd, tl). tl doesn't use anything added to scope in hd so can just overwrite.
   Makes semantics of the handle case difficult, need to know how variables have previously been bound
   when going to compile handle case, likely by using references as a 'cheat' to scoping in this case.
   TODO: Is it justifiable given how odd the semantics/reasoning is? *)
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

(* TODO: Don't believe CApp case needs to be considered, just cases which constain a linastexpr (so bind stuff)
   Can ignore CFunction since its bindings won't actually be evaluated (function gets lifted in actual wasm)
   CMatchTry can be smarter by tracking which points actually lead to a fail, but for now can just be cautious
   and take sum - safe to over-estimate number needed.
   *)
and count_vars_comp c =
  match c.desc with
  | CIf(_, t, f) -> max (count_vars t) (count_vars f)
  | CFor(_, _, _, _, body) -> (count_vars body) + 1
  | CWhile(_, b) -> count_vars b
  | CSwitch(_, cases, default) ->
   let max_case = List.fold_left max 0 (List.map (fun (_, b) -> count_vars b) cases) in
   (match default with None -> max_case | Some c -> max max_case (count_vars c))
(*  | CApp(_, args) -> List.length args  Don't believe this case is necessary  *)
  (* An over-estimate, safe to do but can get more precise by tracking bindings made when each exit reached *)
  | CMatchTry (_, e1, e2) -> (count_vars e1) + (count_vars e2)
  | _ -> 0
