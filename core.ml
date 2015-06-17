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
  | _ -> 
      raise NoRuleApplies

let rec eval ctx store t =
  try let t',store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> t,store

(* ------------------------   TYPING  ------------------------ *)

(* given an (int*int) list, check if ty matches the first argument *)
let matches arrowList ty =
  let (a,b) = List.hd arrowList in
  match ty with
    VarBind(i) -> i==a && b<=a
  | CmdBind(a',b') -> a==a' && b==b'
  | _ -> raise NoRuleApplies
  
let rec searchType binds l =
  if binds==[] then raise NoRuleApplies
  else if l==[] then List.hd (List.hd binds)
  else
    searchType (List.map List.tl
                         (List.filter (fun x->matches x (List.hd l)) binds))
               (List.tl l)

(* type of an expression, must be a pair (a,b) *)               
let rec typeofexp ctx e =
  match e with
  | VarExpr(s) -> (match getbinding ctx s with
                     VarBind(i) -> CmdBind(i,0))
  | OpExpr(s,l) -> (let tyList = List.map (typeofexp ctx) l in
                    match getbinding ctx s with
                      ArrBind(bb) -> CmdBind(searchType bb tyList))
  | NatExpr(i) -> CmdBind(0,0)

let rec typeofcmd ctx t =
  match t with
  | Assign(_,v,e) -> (
    let a' = (match getbinding ctx v with
                VarBind(i) -> i ) in
    let bind' = typeofexp ctx e in
    (match bind' with
       CmdBind(a,b) -> if a'<=a then (CmdBind(a,b))
                       else (pr (string_of_int a'); pr "<=";
                             pr (string_of_int a); pr "failed...";
                             raise NoRuleApplies)))
  | While(_,e,c) -> (let te = match typeofexp ctx e with
                         CmdBind(t1,t2) -> (t1,t2) in
                     let tc = match typeofcmd ctx c with
                         CmdBind(t1,t2) -> (t1,t2) in
                     if (fst te==snd te && snd tc<fst tc) then
                       CmdBind(tc)
                     else raise NoRuleApplies)
  | If(_,e,c,c') -> (let te = match typeofexp ctx e with
                         CmdBind(t1,t2) -> (t1,t2) in
                     let tc = match typeofcmd ctx c with
                         CmdBind(t1,t2) -> (t1,t2) in
                     let tc' = match typeofcmd ctx c' with
                         CmdBind(t1,t2) -> (t1,t2) in
                     if (tc==tc' && fst tc<=fst te) then
                       CmdBind(tc)
                     else raise NoRuleApplies)
  | CmdList(_,cs) -> CmdBind(
                         List.fold_left
                           (fun (a,b) (c,d) -> (max a c, max b d))
                           (0,0)
                           (List.map (fun c->
                                     match typeofcmd ctx c with
                                       CmdBind(a,b)->(a,b))
                                     cs))
  | _ -> raise NoRuleApplies

