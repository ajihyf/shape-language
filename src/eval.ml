open Expr

exception Error

module Ctx = struct
  module StringMap = Map.Make (String)
  type ctx = t_ty StringMap.t
  let empty : ctx = StringMap.empty
  let extend name v ctx =
    if StringMap.mem name ctx then raise Error else
      StringMap.add name v ctx
  let lookup name ctx = StringMap.find name ctx
  let map f ctx = StringMap.map f ctx
  let fold f ctx init = StringMap.fold f ctx init
end

let isval t =
  match t with
    SVar(_) -> true
  | SBool(_) -> true
  | SInt(_) -> true
  | SShape(_) -> true
  | _ -> false

let rec is_true ctx t =
  match t with
    SBool(v) -> v
  | SVar(name) -> is_true ctx (Ctx.lookup name ctx)
  | _ -> false

let rec is_false ctx t =
  match t with
    SBool(v) -> not v
  | SVar(name) -> is_false ctx (Ctx.lookup name ctx)
  | _ -> false

let rec eval1 ctx t =
  match t with
    SVar(name) ->
    Ctx.lookup name ctx
  | SIf(SBool(v), t2, t3) ->
    if v then t2 else t3
  | SIf(t1, t2, t3) ->
    let t1' = eval1 ctx t1 in
    SIf(t1', t2, t3)
  | SLet(t1, t2, t3) when isval t2 ->
    let new_ctx = Ctx.extend t1 t2 ctx in
    eval1 new_ctx t3
  | SLet(t1, t2, t3) ->
    let t2' = eval1 ctx t2 in
    SLet(t1, t2', t3)
  | SCast(t1, t2, t3) when isval t1 ->
    t1
  | SCast(t1, t2, t3) ->
    let t1' = eval1 ctx t1 in
    SCast(t1', t2, t3)
  | _ -> raise Error

let rec eval ctx t =
  try
    let t' = eval1 ctx t
    in eval ctx t'
  with Error -> t
