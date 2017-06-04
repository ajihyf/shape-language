open Expr
open Infer

let builtins = [
  ("not", "bool -> bool");
  ("and", "(bool, bool) -> bool");
  ("or", "(bool, bool) -> bool");

  ("==", "forall[a] (a, a) -> bool");
  ("!=", "forall[a] (a, a) -> bool");

  ("<", "(int, int) -> bool");
  (">", "(int, int) -> bool");
  ("<=", "(int, int) -> bool");
  (">=", "(int, int) -> bool");

  ("+", "(int, int) -> int");
  ("-", "(int, int) -> int");
  ("*", "(int, int) -> int");
  ("/", "(int, i : int | i != 0) -> int");
  ("%", "(int, i : int | i != 0) -> int");
  ("unary-", "int -> int");
]

let uninterpreted = [
  ("left", "(s: shape) -> (l: int | l >= 0)");
  ("top", "(s: shape) -> (l: int | l >= 0)");
  ("width", "(s: shape) -> (l: int | l >= 0)");
  ("height", "(s: shape) -> (l: int | l >= 0)");
  ("length", "forall[t] (a : array[t]) -> (l : int | l >= 0)");
  ("is_prime", "(i : int | i >= 1) -> bool");
]

let primitives = [
  ("my_min", "(a: int, b: int) -> (c: int | c==(if a > b then b else a))");
  ("my_max", "(a: int, b: int) -> (c: int | c==(if a > b then a else b))");
  ("my_abs", "(a: int) -> (c: int | c==(if a >=0 then a else -a))");

  (*("line", "(p1x: int | p1x >= 0, p1y: int | p1y >= 0, p2x: int | p2x >= 0, p2y: int | p2y >= 0) ->
    (s: shape | (left(s)==my_min(p1x,p2x) and top(s)==my_min(p1y,p2y)) and (width(s)==my_abs(p2x-p1x) and height(s)==my_abs(p2y-p1y)))");
  ("rect", "(l: int | l >= 0, t: int | t >= 0, w: int | w >= 0, h: int | h >= 0)" ^
           "-> (s: shape | (left(s)==l and top(s)==t) and (width(s)==w and height(s)==h))");
  ("triangle", "(p1x: int | p1x >=0, p1y: int | p1y >= 0, p2x: int | p2x >= 0, p2y: int | p2y >= 0, p3x: int | p3x >= 0, p3y: int | p3y >= 0)" ^
               "-> (s: shape | (left(s)==my_min(my_min(p1x, p2x), p3x) and top(s)==my_min(my_min(p1y, p2y), p3y)) and " ^
               "(width(s)==my_max(my_abs(p3x-p1x), my_abs(p2x-p1x)) and height(s)==my_max(my_abs(p3y-p1y), my_abs(p2y-p1y))))");
  ("eclipse", "(cx: int | cx > 0, cy: int | cy > 0, rw: int | (rw > 0 and rw <= cx), rh: int | (rh > 0 and rh <= cy))
              -> (s: shape | (left(s)==(cx-rw) and top(s)==(cy-rh)) and (width(s)==2*rw and height(s)==2*rh))");
  ("circle", "(cx: int | cx >= 0, cy: int | cy >= 0, r: int | (r > 0) and (r < cx and r < cy)) " ^
             "-> (s: shape | (left(s)==(cx-r) and top(s)==(cy-r)) and (width(s)==r and height(s)==r))");*)

  ("get", "forall[t] (a : array[t], i : int | i >= 0 and i < length(a)) -> t");
  ("alloc", "forall[t] (i : int) -> (a : array[t] | length(a) == i)");
  ("memcpy", "(dst : array[byte], src : array[byte],
num : int | num <= length(dst) and num <= length(src)) -> unit");
  ("head", "forall[t] (a : array[t] | length(a) > 0) -> t");
  ("is_empty", "forall[t] (a : array[t]) -> (b : bool | b == (length(a) == 0))");
  ("head1", "forall[t] (a : array[t] | not is_empty(a)) -> t");
  ("fac", "(i : int | i >= 0) -> (j : int | j > 0 and j >= i)");
  ("succ", "(i : int) -> (j : int | j == i + 1)");
  ("square", "(i : int) -> (j : int | j == i * i)");
  ("random1toN", "(N : int | N >= 1) -> (i : int | 1 <= i and i <= N)");
  ("my_not", "(b : bool) -> (c : bool | c == (not b))");

  ("make_const", "forall[a b] (x : a) -> b -> (y : a | y == x)");

  ("pair", "forall[t s] (t, s) -> pair[t, s]");
  ("first", "forall[t s] pair[t, s] -> t");
  ("second", "forall[t s] pair[t, s] -> s");

  ("cons", "forall[a] (a, list[a]) -> list[a]");
  ("cons_curry", "forall[a] a -> list[a] -> list[a]");
  ("nil", "forall[a] list[a]");

  ("id", "forall[a] a -> a");
  ("choose", "forall[a] (x : a, y : a) -> (z : a | z == x or z == y)");
  ("plain_choose", "forall[a] (a, a) -> a");
  ("choose_curry", "forall[a] (x : a) -> (y : a) -> (z : a | z == x or z == y)");
  ("plain_choose_curry", "forall[a] a -> a -> a");
]


let env =
  let f env (var_name, ty_str) =
    let ty = Parser.ty_forall_eof Lexer.token (Lexing.from_string ty_str) in
    let t_ty = infer_ty env 0 ty in
    Env.extend var_name t_ty env
  in
  List.fold_left f Env.empty (List.flatten [builtins; uninterpreted; primitives])

let plain_env = Env.map strip_refined_types env
