open Linast
open Types

let defaultLoc = Location.none
let defaultEnv = Env.empty

exception NotImplemented
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
  let makeblock i args = mk (CMakeBlock (i, args))
  let gettag imm = mk (CGetTag imm)
  let mkif imm e1 e2 = mk (CIf (imm, e1, e2))
  let mkwhile imm e = mk (CWhile (imm, e))
  let mkfor id start stop dir e = mk (CFor (id, start, stop, dir, e))
  let mkswitch imm cases partial = mk (CSwitch (imm, cases, partial))
  let matchtry i e1 e2 = mk (CMatchTry (i, e1, e2))
 (* let matchtry e1 e2 = mk (CMatchTry (e1, e2)) *)
  let app imm args = mk (CApp (imm, args))
  let mkfun params e = mk (CFunction (params, e))
end

module LinastExpr = struct
  let mk d : linast_expr =
      {desc=d;
       loc=defaultLoc;
       env=defaultEnv;
       annotations=ref []}
  let mklet rec_flag global binds body = mk (LLet (rec_flag, global, binds, body))
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
  | BLetExport of Asttypes.rec_flag * (Ident.t * compound_expr) list
  (* Cases where binding can fail on first pattern, in which case binding retried on second. Same as CMatchTry *)
  (* Only case of recursion in making bindings *)
  (* Useful where we want to bind stuff, then run same thing after in either case
     e.g. A(x) | B(x) -> e  should be Seq(Try(A case, B case); e)*)
  | BTry of int32 * (linast_setup list) * (linast_setup list)


let rec binds_to_anf binds compound =
  List.fold_right (fun bind body ->
     match bind with
     | BEffect comp -> LinastExpr.seq comp body
     | BLet(id, comp) -> LinastExpr.mklet Nonrecursive Local [(id, comp)] body
     | BLetRec lets -> LinastExpr.mklet Recursive Local lets body
     (* TODO: Seems messy, should be a way to make neater. Have a look at the trees produced *)
     (* Still doesn't achieve as intended - should just get rid of this and call binds_to_anf to put
        trees in compound, rather than having BTry *)
     | BTry (i, binds1, binds2) ->
       LinastExpr.seq
        (Compound.matchtry i
         (binds_to_anf binds1 (Compound.makeblock 0l [])) (* Have to put dummy unit at end of tree *)
         (binds_to_anf binds2 (Compound.makeblock 0l []))
        )
        body
     (* TODO: May want to only allow compiling this if at top level *)
     | BLetExport(flag, lets) -> LinastExpr.mklet flag Global lets body)
    binds (LinastExpr.compound compound)

(* Primative name -> Ident *)
let primIds : (string * Ident.t) list ref = ref []
(* List of Binds to include in program *)
let primBinds : linast_setup list ref = ref []