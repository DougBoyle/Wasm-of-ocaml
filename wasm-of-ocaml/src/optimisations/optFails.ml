(* Note: 'let x = fail i in e' and 'seq(fail i, e)' both never actually occur. Included just to be safe *)
open Linast
open LinastUtils

let will_fail i annotations =  List.mem (Fail i) !annotations
(* Assume no fail annotations before optimisation executes, so don't check if annotation present *)
let mark_failing i annotations = annotations := (Fail i)::(!annotations)
(* If guarenteed to fail, can only be failing with 1 index so only need to find 1 thing *)
let copy_fail from_annots to_annots =
  match List.find_opt (function Fail _ -> true | _ -> false) (!from_annots) with
  | Some (Fail i) -> mark_failing i to_annots
  | _ -> ()

let rec rewrite_fail i body handler = match body.desc with
  | LCompound {c_desc=CMatchFail j}
  (* Below cases should never actually occur, just included for completeness.
     Could technically include lletrec too, but that only ever binds functions *)
  | LSeq ({c_desc=CMatchFail j}, _) | LLet (_, _, {c_desc=CMatchFail j}, _) when i = j -> handler
  (* Assume that body will trigger fail i (so no guards), else rewrite_fail should not have been called *)
  | LSeq (compound, body) -> LinastExpr.seq compound (rewrite_fail i body handler)
  | LLet (id, global, compound, body) ->
    LinastExpr.mklet id global compound (rewrite_fail i body handler)
  | LLetRec (binds, body) -> LinastExpr.mkletrec binds (rewrite_fail i body handler)
  | _ -> failwith "expression not expected to fail"

(* Replace unnecessary MatchTry with rewriten linast *)
let rewrite_linast linast = match linast.desc with
  | LCompound {c_desc=CMatchTry (i, body, handler)} when will_fail i body.annotations ->
    rewrite_fail i body handler
  | LLet(id, global, {c_desc=CMatchTry (i, body, handler)}, rest) when will_fail i body.annotations ->
    OptConstants.rewrite_tree (LinastExpr.mklet id global) (rewrite_fail i body handler) rest
  | LSeq({c_desc=CMatchTry (i, body, handler)}, rest) when will_fail i body.annotations ->
    OptConstants.rewrite_tree LinastExpr.seq (rewrite_fail i body handler) rest
  | _ -> linast

(* Pulling a linast out of a compound, so have to do at linast level *)
(* Using the same functions as in optConstants to put rewritten matchtry linast into overall tree *)
(* Rewriting currently completely discards original linast and any annotations that were on it *)
(* Propagate up fail annotations (after any rewriting) *)
let leave_linast linast =
  let linast = rewrite_linast linast in match linast.desc with
  | LCompound {c_desc=CMatchFail i} -> mark_failing i linast.annotations; linast
  (* These cases should never actually occur, just included for completeness.
     If they did occur, can simplify to compounds since rest will never be evaluated. *)
  | LLet(_, _, ({c_desc=CMatchFail i} as comp), _) | LSeq (({c_desc=CMatchFail i} as comp), _) ->
    mark_failing i linast.annotations; {linast with desc=LCompound comp}
  (* Due to 'fail i' only being introduced during pattern matching, if 2nd part of a sequence
     fails then whole sequence must, 1st part cannot be an assignment or other side effect. *)
  | LLet(_, _, _, rest) | LSeq(_, rest) | LLetRec(_, rest) ->
    copy_fail rest.annotations linast.annotations; linast
  | _ -> linast (* Any remaining cases e.g. LCompound with any other compound expression *)

let optimise linast =
  (LinastMap.create_mapper ~leave_linast ()) linast
