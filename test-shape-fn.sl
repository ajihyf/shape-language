let f = fun(a: int | a > 100, b: int | b > 100): (s: shape | left(s)==a and top(s)==b) -> {rect(a,b,100,100),circle(a+50,b+50,50)} in f(200,200);
