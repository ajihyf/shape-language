let rec f = fun(s: shape, a: int | a > 2 and a == top(s)): (r: shape | top(r) > 0) ->
 (let ret = {s, rect(a - 1, a - 1, 1, 1)} in
 if a > 3 then f(ret, a - 1) else ret)
in let final = f(rect(5, 5, 2, 2), 5) in final