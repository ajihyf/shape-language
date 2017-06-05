open OUnit2
open Expr
open Printing
open Eval

type result =
  | OK of s_expr
  | Fail

let test_cases = [  ("3 + 2", OK(SInt 5));
  ("let x = 1 in x", OK(SInt 1));
  ("let x = 1 in x + 1", OK(SInt 2));
  ("let f = fun (x : int) -> 1 + x in f(1)", OK(SInt 2));
  ("let f = fun (x : int | x > 0, y : int | y == x + 1) -> x + 1 in f(1, 2)", OK(SInt 2));
  ("let f = fun (x : int | x > 3) : (z : int | z > 0) -> x - 1 in f(4)", OK(SInt 3));
    ("let x = 1 in let y = 2 in x + y", OK(SInt 3));
  ("let f = fun (x: Int) -> x + 1 in let g = f(0) in if g > 0 then 1 else 2", OK(SInt 1));
  ("let x = 1 in rect(x, 2, 1, 2)", OK(SRect(SInt 1, SInt 2, SInt 1, SInt 2)));
  ("let s = {rect(1,1,1,1)} in s", OK(SShape([SRect(SInt 1, SInt 1, SInt 1, SInt 1)], false)));
  ("let a = 1+1 in let s = {circle(1,1,1),triangle(a,1,1,1,1,1),rect(2,a,1,1)} in s",
    OK(SShape([SCircle(SInt 1, SInt 1, SInt 1); STriangle(SInt 2, SInt 1, SInt 1, SInt 1, SInt 1, SInt 1);
               SRect(SInt 2, SInt 2, SInt 1, SInt 1)], false)));
  ("let f = fun(x: Int->Int) -> x in let g = fun(x: Int) -> x + 1 in let m = f(g) in m(1)", OK(SInt 2));
  ("let a = 1+1 in let s = {circle(1,1,1),triangle(a,1,1,1,1,1),rect(2,a,a,1)} in s",
    OK(SShape([SCircle(SInt 1, SInt 1, SInt 1); STriangle(SInt 2, SInt 1, SInt 1, SInt 1, SInt 1, SInt 1);
               SRect(SInt 2, SInt 2, SInt 2, SInt 1)], false)));
  ("let f = fun(x: Int->Int) -> x in let g = fun(x: Int) -> x + 1 in let m = f(g) in m(1)", OK(SInt 2));
  ("let rec f = fun(x: Int->Int) -> x in let g = fun(x: Int) -> x + 1 in let m = f(g) in m(1)", OK(SInt 2));
  ("let rec f = fun(s: shape, a: int | a > 2 and a == top(s)): (r: shape | top(r) > 0) -> " ^
   " (let ret = {s, rect(a - 1, a - 1, 1, 1)} in" ^
   " if a > 3 then f(ret, a - 1) else ret) in let final = f(rect(5, 5, 2, 2), 5) in final",
   OK(SShape([SShape([SShape([SRect(SInt 5, SInt 5, SInt 2, SInt 2); SRect(SInt 4, SInt 4, SInt 1, SInt 1)],false);
   SRect(SInt 3, SInt 3, SInt 1, SInt 1)], false);
   SRect(SInt 2, SInt 2, SInt 1, SInt 1)], false)));
   ("let rec f = fun(a: int | a >= 0 and a <= 300):
        (r: shape | (left(r)==a and top(r)==a) and (width(r)<=500-a and height(r)<=500-a)) -> (
    let ret = rect(a, a, 50, 50) in
    if a < 100 then {$ret, f(a + 50)} else line(a, a, a, a)
    ) in f(50)", OK(SShape([SRect(SInt 50, SInt 50, SInt 50, SInt 50);
                            SLine(SInt 100, SInt 100, SInt 100, SInt 100)], true)));
    ("let eyes = fun(centerx: int | centerx >= 160):" ^
     "(r: shape | (left(r)==centerx-160 and width(r)==320) and (top(r)==200 and height(r)==40)) -> (" ^
     "{$rect(centerx-160,200,80,40),rect(centerx+80,200,80,40)}" ^
     ") in {$eyes(300),triangle(300,50,250,100,350,100)}",
     OK(SShape([SShape([SRect(SInt 140, SInt 200, SInt 80, SInt 40); SRect(SInt 380, SInt 200, SInt 80, SInt 40)],true)
       ;STriangle(SInt 300, SInt 50, SInt 250, SInt 100, SInt 350, SInt 100)],true)));
]


let string_of_result = function
  | Fail -> "Fail"
  | OK s_expr -> "OK (" ^ string_of_s_expr s_expr ^ ")"

let make_single_test_case (code, expected_result) =
  String.escaped code >:: fun _ ->
    Infer.reset_id () ;
    let result =
      try
        OK (eval Ctx.StringMap.empty (Parser.expr_eof Lexer.token (Lexing.from_string code)))
      with Parsing.Parse_error ->
        Fail
    in
    assert_equal ~printer:string_of_result expected_result result ;
    match result with
    | OK s_expr -> begin
        let s_expr_str = string_of_s_expr s_expr in
        Infer.reset_id () ;
        try
          let new_result = OK (Parser.expr_eof Lexer.token (Lexing.from_string s_expr_str)) in
          assert_equal ~printer:string_of_result ~msg:"string_of_s_expr error"
            expected_result new_result
        with Parsing.Parse_error ->
          assert_failure ("string_of_s_expr parsing error: " ^ s_expr_str)
      end
    | Fail -> ()

let suite =
  "test_eval" >::: List.map make_single_test_case test_cases
