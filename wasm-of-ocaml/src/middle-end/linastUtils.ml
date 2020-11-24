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

(* TODO: Helper functions to enable translating to wasm (and also optimisation passes later) *)
let free_vars ast = raise (NotImplemented __LOC__)
(* TODO: Work out logic behind counting, why is this the number needed? Number of vars needed at any 1 point *)
(* Assumption that all vars can use the same type of local variable - is this true/relevant? *)
let rec count_vars (ast : linast_expr) = match ast.desc with
  | LLet (_, _, comp, body) -> max (count_vars_comp comp) (1 + count_vars body)
  | LLetRec (binds, body) ->
    let max_binds = List.fold_left max 0 (List.map (fun (_, _, c) -> count_vars_comp c) binds) in
    max max_binds (List.length binds + (count_vars body))
   | LSeq(comp, linast) -> max (count_vars_comp comp) (count_vars linast)
   | LCompound c -> count_vars_comp c

and count_vars_comp c =
  match c.desc with
  | CIf(_, t, f) -> max (count_vars t) (count_vars f)
  | CWhile(_, b) -> count_vars b
  | CSwitch(_, cases, default) ->
   let max_case = List.fold_left max 0 (List.map (fun (_, b) -> count_vars b) cases) in
   (match default with None -> max_case | Some c -> max max_case (count_vars c))
  | CApp(_, args) -> List.length args (* Why do args matter but not functions *)
  (* TODO: Check the semantics here - Currently bindings can carry over so have to take the sum??
           Ignores the fact that some variables could be repeated in places in which case double counted.
           May need smarter method. Very conservative for now *)
  | CMatchTry (_, e1, e2) -> (count_vars e1) + (count_vars e2)
  | _ -> 0
