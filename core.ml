open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmTag(_,l,t1,_) -> isval ctx t1
  | TmString _  -> true
  | TmUnit(_)  -> true
  | TmLoc(_,_) -> true
  | TmFloat _  -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | _ -> false

type store = term list  
let emptystore = []
let extendstore store v = (List.length store, List.append store [v])
let lookuploc store l = List.nth store l
let updatestore store n v =
  let rec f s = match s with 
      (0, v'::rest) -> v::rest
    | (n, v'::rest) -> v' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in
    f (n,store)
(* let shiftstore i store = List.map (fun t -> termShift i t) store  *)

exception NoRuleApplies

let rec eval1 ctx store t = match t with
(*    TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12, store
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2',store' = eval1 ctx store t2 in
      TmApp(fi, v1, t2'), store'
  | TmApp(fi,t1,t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmApp(fi, t1', t2), store'
  | TmIf(_,TmTrue(_),t2,t3) ->
      t2, store
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3, store
  | TmIf(fi,t1,t2,t3) ->
      let t1',store' = eval1 ctx store t1 in
      TmIf(fi, t1', t2, t3), store'
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2, store 
  | TmLet(fi,x,t1,t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmLet(fi, x, t1', t2), store' 
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12, store
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1',store' = eval1 ctx store t1
      in TmFix(fi,t1'), store'
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest',store' = evalafield rest in
          (l,vi)::rest', store'
      | (l,ti)::rest -> 
          let ti',store' = eval1 ctx store ti in
          (l, ti')::rest, store'
      in let fields',store' = evalafield fields in
      TmRecord(fi, fields'), store'
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields, store
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1',store' = eval1 ctx store t1 in
      TmProj(fi, t1', l), store'
  | TmTag(fi,l,t1,tyT) ->
      let t1',store' = eval1 ctx store t1 in
      TmTag(fi, l, t1',tyT), store'
  | TmCase(fi,TmTag(_,li,v11,_),branches) when isval ctx v11->
      (try 
         let (x,body) = List.assoc li branches in
         termSubstTop v11 body, store
       with Not_found -> raise NoRuleApplies)
  | TmCase(fi,t1,branches) ->
      let t1',store' = eval1 ctx store t1 in
      TmCase(fi, t1', branches), store'
  | TmAscribe(fi,v1,tyT) when isval ctx v1 ->
      v1, store
  | TmAscribe(fi,t1,tyT) ->
      let t1',store' = eval1 ctx store t1 in
      TmAscribe(fi,t1',tyT), store'
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t,store 
        | _ -> raise NoRuleApplies)
  | TmRef(fi,t1) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmRef(fi,t1'), store')
      else
        let (l,store') = extendstore store t1 in
        (TmLoc(dummyinfo,l), store')
  | TmDeref(fi,t1) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmDeref(fi,t1'), store')
      else (match t1 with
            TmLoc(_,l) -> (lookuploc store l, store)
          | _ -> raise NoRuleApplies)
  | TmAssign(fi,t1,t2) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmAssign(fi,t1',t2), store')
      else if not (isval ctx t2) then
        let (t2',store') = eval1 ctx store t2
        in (TmAssign(fi,t1,t2'), store')
      else (match t1 with
            TmLoc(_,l) -> (TmUnit(dummyinfo), updatestore store l t2)
          | _ -> raise NoRuleApplies)
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2), store
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2',store' = eval1 ctx store t2 in
      TmTimesfloat(fi,t1,t2') , store'
  | TmTimesfloat(fi,t1,t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmTimesfloat(fi,t1',t2) , store'
  | TmSucc(fi,t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmSucc(fi, t1'), store'
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo), store
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1, store
  | TmPred(fi,t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmPred(fi, t1'), store'
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo), store
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo), store
  | TmIsZero(fi,t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmIsZero(fi, t1'), store' *)
  | _ -> 
      raise NoRuleApplies

let rec eval ctx store t =
  try let t',store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> t,store

(* ------------------------   TYPING  ------------------------ *)

let rec typeofexp ctx e =
  match e with
  | VarExpr(s) -> (match getbinding ctx s with
                     VarBind(i) -> CmdBind(i,0))
  | OpExpr(s,l) -> CmdBind(1,0)
  | NatExpr(i) -> CmdBind(1,0)

let rec typeofcmd ctx t =
  match t with
  | Assign(_,v,e) -> (
    let a' = (match getbinding ctx v with
                VarBind(i) -> i ) in
    let bind' = typeofexp ctx e in
    (match bind' with
       CmdBind(a,b) -> if a'<a then (CmdBind(a,b))
                       else raise NoRuleApplies))
  | While(_,e,c) -> VarBind(0)
  | If(_,e,c,c') -> VarBind(0)
  | CmdList(_,cs) -> VarBind(0)
  | _ -> raise NoRuleApplies

