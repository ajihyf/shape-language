type name = string
type id = int
type level = int


(* Types *)
type 'a ty =
  | TConst of name
  | TApp of name * 'a ty list
  | TArrow of ('a refined_ty) list * ('a refined_ty)
  | TVar of ('a tvar) ref

and 'a refined_ty =
  | Plain of 'a ty
  | Named of name * 'a ty
  | Refined of name * 'a ty * 'a

and 'a tvar =
  | Unbound of id * level
  | Link of 'a ty
  | Generic of id

type s_ty = s_expr ty
and s_refined_ty = s_expr refined_ty

and s_expr =
  | SVar of name
  | SBool of bool
  | SInt of int
  | SCall of s_expr * s_expr list
  | SFun of s_param list * s_refined_ty option * s_expr
  | SLet of name * s_expr * s_expr
  | SIf of s_expr * s_expr * s_expr
  | SCast of s_expr * s_ty * s_expr option
  | SFix of s_expr
  | SShape of s_expr list

and s_param = name * (s_ty * s_expr option) option


(* Typed expressions *)
type t_ty = t_expr ty
and t_refined_ty = t_expr refined_ty

and t_expr = {shape : t_expr_shape; ty : t_ty}

and t_expr_shape =
  | EVar of name
  | EBool of bool
  | EInt of int
  | ECall of t_expr * t_expr list
  | EFun of t_param list * t_refined_ty option * t_expr
  | ELet of name * t_expr * t_expr
  | EIf of t_expr * t_expr * t_expr
  | ECast of t_expr * t_ty * t_expr option
  | EFix of name * t_expr
  | EShape of t_expr list

and t_param = name * t_ty * t_expr option


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
    | _ -> false

let rec beta_reduction ctx para_list val_list exp =
    match (para_list, val_list) with
      ([(name, None)], [y]) -> SLet(name, y, exp)
    | ((name, None)::rest1, y::rest2) ->
      let new_ctx = Ctx.extend name y ctx in
        beta_reduction new_ctx rest1 rest2 exp
    | _ -> raise Error

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
    | SCall(SVar(name), t2) when is_built_in_call name ->
        (match t2 with
           x1::x2::[] ->
            (match (name, x1, x2) with
              ("+", SInt(v1), SInt(v2)) -> SInt(v1 + v2)
            | ("-", SInt(v1), SInt(v2)) -> SInt(v1 - v2)
            | _ -> raise Error)
         | _ -> raise Error)
    | SCall(SVar(name), t2) ->
        (match Ctx.lookup name ctx with
          SFun(param_list, _, exp) -> beta_reduction ctx param_list t2 exp
        | _ -> raise Error)
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
    with Error -> t;;

let a = eval Ctx.StringMap.empty (SIf(SBool(false), SInt(1), SCall(SVar("+"), [SInt(1); SInt(2)]))) in
    match a with
    SBool(_) -> print_string("True")
    | SCall(_, _) -> print_string("False")
    | _ -> print_string("Shall not see this")
