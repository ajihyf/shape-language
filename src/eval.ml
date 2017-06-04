open Expr

exception Error of string

module Ctx = struct
  module StringMap = Map.Make (String)
  type ctx = t_ty StringMap.t
  let empty : ctx = StringMap.empty
  let extend name v ctx =
    if StringMap.mem name ctx then raise (Error "the name is allocated already") else
      StringMap.add name v ctx
  let lookup name ctx = StringMap.find name ctx
  let map f ctx = StringMap.map f ctx
  let fold f ctx init = StringMap.fold f ctx init
end

let isval t =
    match t with
      SBool(_) -> true
    | SInt(_) -> true
    | SFun(_,_,_) -> true
    | _ -> false

let rec list_isval t =
    match t with
      [SBool(_)] -> true
    | [SInt(_)] -> true
    | [SFun(_,_,_)] -> true
    | SBool(_)::rest -> list_isval rest
    | SInt(_)::rest -> list_isval rest
    | SFun(_,_,_)::rest -> list_isval rest
    | _ -> false

let rec is_true ctx t =
    match t with
      SBool(v) -> v
    | SVar(name) -> is_true ctx (Ctx.lookup name ctx)
    | _ -> false

let rec is_false ctx t =
    match t with
      SBool(v) ->
        if v then false else true
    | SVar(name) -> is_false ctx (Ctx.lookup name ctx)
    | _ -> false

let is_built_in_call name =
    match name with
      "+" -> true
    | "-" -> true
    | ">" -> true
    | "<" -> true
    | "==" -> true
    | _ -> false

let rec beta_reduction ctx para_list val_list exp =
    match (para_list, val_list) with
      ([(name, None)], [y]) -> SLet(name, y, exp)
    | ([(name, Some (param_s_ty, None))], [y]) -> SLet(name, y, exp)
    | ([(name, Some (param_s_ty, Some contract_s_expr))], [y]) -> SLet(name, y, exp)
    | ((name, None)::rest1, y::rest2) ->
      let new_ctx = Ctx.extend name y ctx in
        SLet(name, y, (beta_reduction new_ctx rest1 rest2 exp))
    | ((name, Some (param_s_ty, None))::rest1, y::rest2) ->
      let new_ctx = Ctx.extend name y ctx in
        SLet(name, y, (beta_reduction new_ctx rest1 rest2 exp))
    | ((name, Some (param_s_ty, Some contract_s_expr))::rest1, y::rest2) ->
      let new_ctx = Ctx.extend name y ctx in
        SLet(name, y, (beta_reduction new_ctx rest1 rest2 exp))
    | _ -> raise (Error "beta_reduction error")

let rec replace_var ctx expr =
    match expr with
      SVar(name) -> Ctx.lookup name ctx
    | _ -> expr

let rec replace_var_in_list ctx expr_list =
    match expr_list with
      [] -> []
    | x::rest -> (replace_var ctx x)::(replace_var_in_list ctx rest)

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
    | SCall(SVar(name), t2) when (not (list_isval t2)) ->
        let t2' = replace_var_in_list ctx t2 in
          eval1 ctx (SCall(SVar(name), t2'))
    | SCall(SVar(name), t2) when is_built_in_call name ->
        (match t2 with
           x1::x2::[] ->
            (match (name, x1, x2) with
              ("+", SInt(v1), SInt(v2)) -> SInt(v1 + v2)
            | ("-", SInt(v1), SInt(v2)) -> SInt(v1 - v2)
            | (">", SInt(v1), SInt(v2)) ->
                if v1 > v2 then SBool(true) else SBool(false)
            | ("<", SInt(v1), SInt(v2)) ->
                if v1 < v2 then SBool(true) else SBool(false)
            | ("==", SInt(v1), SInt(v2)) ->
                if v1 == v2 then SBool(true) else SBool(false)
            | _ -> raise (Error "parameter of +/- is not int"))
         | _ -> raise (Error "parameter num of +/- is not 2"))
    | SCall(SVar(name), t2) ->
        (match Ctx.lookup name ctx with
          SFun(param_list, _, exp) -> beta_reduction ctx param_list t2 exp
        | _ -> raise (Error "call an undefined function"))
    | SCast(t1, t2, t3) when isval t1 -> t1
    | SCast(t1, t2, t3) ->
        let t1' = eval1 ctx t1 in
            SCast(t1', t2, t3)
    | SFun(t1, t2, t3) -> SFun(t1, t2, t3)
    | _ -> raise (Error "no rule to apply")

let rec eval ctx t =
    try
        let t' = eval1 ctx t
        in eval ctx t'
    with (Error "no rule to apply") -> t
