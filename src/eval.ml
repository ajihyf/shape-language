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
      SBool(_) -> true
    | SInt(_) -> true
    | _ -> false

let is_true t =
    match t with
     SBool(v) -> if v then true else false
    | _ -> false

let is_false t =
    match t with
      SBool(v) -> if v then false else true
    | _ -> false

let rec eval1 ctx t =
    match t with
      SIf(SBool(v), t1, t2) ->
        if v then t1 else t2
    | SLet(name, t1, t2) when isval t1 ->
        let new_ctx = Ctx.extend name t1 ctx in
            eval1 new_ctx t2
    | SLet(name, t1, t2) ->
        let t1' = eval1 ctx t1 in
            SLet(name, t1', t2)
    | _ -> raise Error

let rec eval ctx t =
    try
        let t' = eval1 ctx t
        in eval ctx t'
    with Error -> t