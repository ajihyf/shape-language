2333;
testsum = 100;

let f = fun(a: int | a > 4): (b: int | b > a) -> a + 1 in f: (a: int | a > 5) -> (b: int | b >= a);

let rec sum = fun(a: int | a >= 1): (r: int | r >= a) -> (
    if a > 1 then a + sum(a - 1) else a
) in sum(testsum);
