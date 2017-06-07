let rec squares = fun(a: int | a > 0, x: int | x > 0):
        (r: shape | (left(r)==x and top(r)==x) and (width(r)==a and height(r)==a)) -> (
    let ret = rect(x, x, a, a) in
    if a > 10 then {ret, squares(a - 10, x)} else ret
) in squares(100, 300);
