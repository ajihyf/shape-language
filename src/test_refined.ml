open OUnit2
open Expr

type result =
  | OK
  | Fail of string option

let fail = Fail None
let error msg = Fail (Some msg)

let wrong = error "SMT solver returned sat."
let unknown = error "SMT solver returned unknown."

let test_cases = [
  ("1 : int | true", OK);
  ("1 : int | false", wrong);
  ("1 : int | 1 > 2", wrong);
  ("1 : int | not 1 > 2", OK);
  ("1 : int | 1 + 5 > 2", OK);
  ("1 : int | 1 != 0 and - 2 <= 2 - 1", OK);
  ("let x = 1 in 1 : int | x > 2", wrong);
  ("let x = 1 in 1 : int | x + 3 > 2", OK);
  ("1 / 0", wrong);
  ("let x = 0 in 1 / x", wrong);
  ("1 / (4 % 2)", wrong);
  ("1 / (4 % 3)", OK);
  ("1 / 1", OK);
  ("let x = 1 in 1 / x", OK);
  ("1 / succ(0)", OK);
  ("if 1 == 2 then 1 / 0 else 1", OK);
  ("random1toN(-1)", wrong);
  ("1 / random1toN(5)", OK);
  ("if 2 > 1 then 1 else 1 / 0", OK);
  ("let x = random1toN(10) in if x > 5 then 1 / (x - 5) else 1 / (6 - x)", OK);
  ("let x = random1toN(1000) in " ^
   " let z = " ^
   "  if x > 10 then " ^
   "   let n = random1toN(x) in " ^
   "   n - 2 * x " ^
   "  else " ^
   "   let m = random1toN(10) + 10 in " ^
   "   m - x " ^
   " in " ^
   " 1 / z", OK);
  ("let x = random1toN(100) - 10 in let y = random1toN(100) - 50 in " ^
   "if x > 0 then " ^
   " if y > 0 then " ^
   "  1 / x + 1 / y " ^
   " else " ^
   "  1 / x + y " ^
   "else " ^
   " x + y", OK);
  ("fun (x : int) -> 1 / x", wrong);
  ("fun (x : int | x > 0) -> 1 / x", OK);
  ("fun (x : int | x > 2) -> x : int | x > 0", OK);
  ("fun (x : int | x > 0) -> x : int | x > 4", wrong);
  ("fun (x : int | x > 3) : (z : int | z > 0) -> x - 2", OK);
  ("fun (x : int, y : int) : (z : int | z >= x and z >= y) -> if x <= y then x else y", wrong);
  ("fun (x : int, y : int) : (z : int | z >= x and z >= y) -> if x >= y then x else y", OK);
  ("let a = alloc(5) in 1 / (length(a) - 5)", wrong);
  ("let a = alloc(5) in 1 / (length(a) + 1)", OK);
  ("fun(x : int | x >= 0) -> let y = x + 9 in let z = square(y) in 1 / (z - random1toN(99))",
   wrong);
  ("fun(x : int | x >= 0) -> let y = x + 10 in let z = square(y) in 1 / (z - random1toN(99))",
   OK);
  ("fun(a, def) -> let l = length(a) + 1 in if l >= 1 then get(a, 0) else def", wrong);
  ("fun(a, def) -> let l = length(a) + 1 in if l >= 2 then get(a, 0) else def", OK);
  ("fun(a) -> head(a)", wrong);
  ("fun(a) -> if length(a) >= 1 then head(a) else -1", OK);
  ("fun(a) -> if length(a) >= 1 then head1(a) else -1", OK);
  ("fun(a) -> if not is_empty(a) then head1(a) else -1", OK);
  ("fun(a) -> if my_not(is_empty(a)) then head1(a) else -1", OK);
  ("fun(a) -> if is_empty(a) then head1(a) else -1", wrong);
  ("fun(a) -> if is_empty(a) then -1 else head(a)", OK);
  ("fun(a : some[b] b | length(a) >= 1) -> head1(a)", OK);
  ("fun(a : some[b] b | my_not(is_empty(a))) -> head(a)", OK);
  ("1 : int | choose(1, 1) == 1", OK);
  ("let a = plain_choose(1, 1) in a == 1", OK);
  ("let f = choose_curry(2) in " ^
   "let a = f(3) + f(5) in " ^
   "if not ((a == 4 or a == 5) or (a == 7 or a == 8)) " ^
   "then 1 / 0 else 1", OK);
  ("let f = choose_curry(2) in " ^
   "let a = f(3) + f(5) in " ^
   "if not ((a == 3 or a == 5) or (a == 7 or a == 8)) " ^
   "then 1 / 0 else 1", wrong);

  (* nil is a primitive constant *)
  ("if nil == nil then 1 else 0", OK);

  (* Z3 cannot prove this *)
  ("fun(x : int | x > 0, y : int | y > 0, z : int | z > 0) -> 1 / (x*x*x + y*y*y - z*z*z)",
   unknown);

  (* this requires the NLSat solver *)
  ("fun(n : int | n >= 0, m : int | m >= 0, " ^
   "    i : int | 0 <= i and i < m, " ^
   "    j : int | 0 <= j and j < n, " ^
   "    a : array[byte] | length(a) == m * n) -> " ^
   " get(a, i * n + j)", OK);

  (* Heartbleed *)
  ("fun(payload : array[byte], payload_length : int) : array[byte] -> " ^
   " let response = alloc(payload_length) in " ^
   " let ignore = memcpy(response, payload, payload_length) in " ^
   " response", wrong);
  (* Heartbleed fix *)
  ("fun(payload : array[byte], " ^
   "    payload_length : int | length(payload) == payload_length) : array[byte] -> " ^
   " let response = alloc(payload_length) in " ^
   " let ignore = memcpy(response, payload, payload_length) in " ^
   " response", OK);

  (* first class functions *)
  ("let f = succ in 1 : int | f(1) == 2", OK);
  ("let f = succ in 1 : int | f(1) == 3", wrong);
  ("let f = fun(x : int | x > 0) : (y : int | y > 0) -> x + 1 in 1 / f(1)", OK);
  ("let f = fun(x : int | x > 0) : (y : int | y > 0) -> x + 1 in 1 / (f(1) - 1)", wrong);
  ("let f = fun(x : int | x > 0) : (y : int | y > 0) -> x + 1 in f(0)", wrong);
  ("let f = fun(x) : (y : int | y > x) -> x + 1 in 1 / f(0)", OK);
  ("let f = fun(x) : (y : int | y > x) -> x + 1 in 1 / f(-1)", wrong);
  ("let a = 2 in fun(x) : (y : int | y > x) -> x + a", OK);
  ("let a = -1 in fun(x) : (y : int | y > x) -> x + a", wrong);
  ("1 : int | succ(0) == 1", OK);
  ("1 : int | succ(0) == 2", wrong);
  ("let min = fun(x, y) : (z : int | z == (if x > y then y else x)) -> " ^
   " if x > y then y else x " ^
   "in " ^
   "let abs= fun(i) : (n : int | (if i > 0 then n == i else n == -i)) -> -min(-i, i) in " ^
   "abs(-3)", OK);
  (*("let max = fun(x, y) : (z : int | (if x > y then z == x else z == y)) -> " ^
    " if x > y then x else y " ^
    "in " ^
    "let abs = fun(x) : (z : int | z == (if x >= 0 then x else -x)) -> max(x, -x) in " ^
    "fun (x : int | abs(x) <= 10) -> " ^
    " let z = if max(square(x), 25) == 25 then " ^
    "   3 * x + 7 * random1toN(10) " ^
    "  else if x == 11 then " ^
    "   0 " ^
    "  else " ^
    "   x " ^
    " in " ^
    " 100 / z", OK);*)
  ("let max = fun(x, y) : (z : int | (if x > y then z == x else z == y)) -> " ^
   " if x < y then x else y " ^
   "in max", wrong);
  ("let max = fun(x, y) : (z : int | (if x > y then z == x else z == y)) -> " ^
   " if x > y then x else y " ^
   "in 1 : int | max(-7, 3) == 3 and max(15, 3) != 3", OK);
  ("fun x -> if x > 0 then let f = fun y -> y / x in f(2) else 2", OK);
  ("fun x -> if x >= 0 then let f = fun y -> y / x in f(2) else 2", wrong);
  ("let const_1 = make_const(1) in fun a -> 1 : int | const_1(a) == 1", OK);
  ("let const_1 = make_const(1) in fun x -> 1 : int | const_1(x) == 1", OK);
  ("let const_1 = make_const(1) in fun x -> 1 : int | const_1(x) == 2", wrong);
  ("let const_7 = make_const(7) in fun(x, y) -> 1 : int | const_7(x) == const_7(y)", OK);
  ("let test = fun(x : int, y : int | y == x - 1) -> 1 in " ^
   "let const_2 = make_const(2) in " ^
   "let a = 3 in " ^
   "test(a, const_2(a))", OK);
  ("let f = fun(z, y : int | y == z + 1) -> 1 in let x = 2 in f(1, x)", OK);
  ("let f = fun(x, y : int | y == x + 1) -> 1 in let x = 2 in f(1, x)", OK);
  ("let test = fun(i : int | i > -6) : (j : int | j >= i * 3) -> i * 3 + 1 in " ^
   "test : (x : int | x > 0) -> (y : int | y > x)", OK);
  ("let test = fun(i : int | i > 1) : (j : int | j >= i * 3) -> i * 3 + 1 in " ^
   "test : (x : int | x > 0) -> (y : int | y > x)", wrong);
  ("let test = fun(i : int | i > -6) : (j : int | j >= i * 3) -> i * 3 + 1 in " ^
   "test : (x : int | x >= -1) -> (y : int | y > x)", wrong);

  (* function subtyping *)
  ("(fun x -> x + 1) : int -> int", OK);
  ("let f = fun x -> x + 1 in f : int -> int", OK);
  ("let f = fun(x : int | x > 0) : (y : int | y == x + 1) -> x + 1 in " ^
   "f : (x : int | x > 0) -> int", OK);
  ("succ : int -> int | succ(0) == 1", OK);
  ("succ : (x : int | x > 0) -> (y : int | y > 0)", OK);
  ("succ : (x : int | x > 0) -> (y : int | y > 1)", OK);
  ("succ : (x : int | x > 0) -> (y : int | y > 2)", wrong);
  ("let a = 1 in succ : (x : int | x > 0) -> (y : int | y > a)", OK);
  ("let a = 2 in succ : (x : int | x > 0) -> (y : int | y > a)", wrong);
  ("let a = 0 in fac : (x : int | x >= a) -> int", OK);
  ("let a = -1 in fac : (x : int | x >= a) -> int", wrong);
  ("fac : (a : int | a >= 0) -> int", OK);
  ("fac : (a : int | a >= 0) -> (z : int | z > -1)", OK);
  ("let f = fac : (x : int | x > 0) -> (z : int | z > 0) in 1 / f(100)", OK);
  ("let f = fac : (x : int | x > 0) -> (z : int | z > -2) in 1 / f(100)", wrong);
  ("fac(0)", OK);
  ("fac(-1)", wrong);
  ("let f = fac in f(0)", OK);
  ("let f = fac in f(-1)", wrong);
  ("let f = fac : (a : int | a > 0) -> int in f(1)", OK);
  ("let f = fac : (a : int | a > 0) -> int in f(0)", wrong);
  ("fun x -> " ^
   "if x >= 0 then " ^
   " let f = fac : (a : int | a >= x) -> int in " ^
   " f(x + 1) " ^
   "else " ^
   " let f = fac : (a : int | a >= 0) -> (z : int | z > x) in " ^
   " f(-x)", OK);
  ("make_const(1) : int -> (x : int | x >= 0)", OK);
  ("make_const(-1) : int -> (x : int | x >= 0)", wrong);
  ("make_const(1) : int -> (x : int | x >= 2)", wrong);
  ("make_const(1) : int -> (x : int | x == 1)", OK);
  ("make_const(1) : int -> (x : int | x == 0)", wrong);
  ("let min = fun(x, y) : (z : int | z == (if x > y then y else x)) -> " ^
   " if x > y then y else x " ^
   "in " ^
   "min : (i : int | i > 0, j : int | j < 0) -> (k : int | k < 0)", OK);
  ("succ : int -> int | succ(0) == 1", OK);
  ("succ : int -> int | succ(0) == 2", wrong);
  ("succ : (x : int | x > 0) -> (y : int | y > 1) | succ(0) == 1", OK);
  ("succ : (x : int | x > 0) -> (y : int | y > 1) | succ(0) == 2", wrong);
  ("succ : (x : int | x > 0) -> (y : int | y > 2) | succ(0) == 1", wrong);
  (*("fun (a: int -> int) -> (fun (b: int) -> a(1) + b)", OK);*)
  ("let r = rect(1, 2, 3, 4) in r: shape | (left(r) == 1 and top(r) == 2) and (width(r) == 3 and height(r) == 4)", OK);
  ("let r = rect(1, 2, 3, 4) in r: shape | (left(r) == 2 and top(r) == 2) and (width(r) == 3 and height(r) == 4)", wrong);
  ("let r = line(1, 2, 3, 4) in r: shape | (left(r) == 1 and top(r) == 2) and (width(r) == 2 and height(r) == 2)", OK);
  ("let r = line(1, 2, 3, 4) in r: shape | (left(r) == 1 and top(r) == 2) and (width(r) == 3 and height(r) == 3)", wrong);
  ("let r = triangle(1, 2, 3, 4, 5, 6) in r: shape | (left(r) == 1 and top(r) == 2) and (width(r) == 3 and height(r) == 3)", wrong);
  ("let r = triangle(1, 2, 3, 4, 5, 6) in r: shape | (left(r) == 1 and top(r) == 2) and (width(r) == 4 and height(r) == 4)", OK);
  ("let r = circle(1, 2, 3) in r: shape", wrong);
  ("let r = circle(3, 2, 1) in r: shape | (left(r) == 2 and top(r) == 2) and (width(r) == 2 and height(r) == 2)", wrong);
  ("let r = circle(3, 2, 1) in r: shape | (left(r) == 2 and top(r) == 1) and (width(r) == 2 and height(r) == 2)", OK);

  ("let s = {rect(1,2,3,4)} in s: shape", OK);
  ("let s = {rect(1,2,3,4)} in s: shape | (width(s)<=1)", wrong);
  ("let s = {rect(1,2,3,4)} in s: shape | (top(s)==1)", wrong);
  ("let s = {rect(1,2,3,4)} in s: shape | (left(s)==1 and top(s)==2)", OK);
  ("let s = {rect(1,1,1,1),rect(2,2,2,2)} in s: shape | ((left(s)==2 and top(s)==2) and (width(s)==3 and height(s)==3))", wrong);
  ("let s = {rect(1,1,1,1),rect(2,2,2,2)} in s: shape | ((left(s)==1 and top(s)==1) and (width(s)==2 and height(s)==2))", wrong);
  ("let s = {rect(1,1,1,1),rect(2,2,2,2)} in s: shape | ((left(s)==1 and top(s)==1) and (width(s)==3 and height(s)==3))", OK);
  ("let s = {rect(2,3,2,2),line(1,6,2,3)} in s: shape | ((left(s)==1 and top(s)==3) and (width(s)==2 and height(s)==2))", wrong);
  ("let s = {rect(2,3,2,2),line(1,6,2,3)} in s: shape | ((left(s)==1 and top(s)==3) and (width(s)==3 and height(s)==3))", OK);

  ("let s = rect(0,0,0,0) in s: shape", wrong);
  ("let s = circle(1,1,2) in s: shape", wrong);
  ("let s = rect(2,1+1,1,1) in s: shape | (left(s)==2 and top(s)==2)", OK);
  ("let s = {rect(0,0,1,1),rect(1,1,1,1),rect(2,1+1,1,1)} in s: shape | (width(s)==3 and height(s)==3)", OK);

  ("let a = 1+1 in let s = {rect(0,0,1,1),rect(1,1,1,1),rect(2,a,1,1)} in s: shape | (width(s)==3 and height(s)==3)", OK);
]



let string_of_result = function
  | Fail None -> "Fail"
  | Fail (Some msg) -> "Fail " ^ msg
  | OK -> "OK"

let cmp_result result1 result2 = match (result1, result2) with
  | Fail None, Fail _ | Fail _, Fail None -> true
  | Fail (Some msg1), Fail (Some msg2) -> msg1 = msg2
  | OK, OK -> true
  | _ -> false

let make_single_test_case (code, expected_result) =
  String.escaped code >:: fun _ ->
    let result =
      try
        let expr = Parser.expr_eof Lexer.token (Lexing.from_string code) in
        let t_expr = Infer.infer_expr Core.plain_env 0 expr in
        Refined.check_expr t_expr ;
        OK
      with Refined.Error msg ->
        Fail (Some msg)
    in
    assert_equal ~printer:string_of_result ~cmp:cmp_result expected_result result

let suite =
  "test_prove" >::: List.map make_single_test_case test_cases



