2333;
testsum = 100;
let rec sum = fun(a: int | a >= 1): (r: int | r >= a) -> (
    if a > 1 then a + sum(a - 1) else a
) in sum(testsum);

let rec f = fun(a: int | a >= 0 and a <= 300):
        (r: shape | (left(r)==a and top(r)==a) and (width(r)<=500-a and height(r)<=500-a)) -> (
    let ret = rect(a, a, 50, 50) in
    if a < 100 then {$ret, f(a + 50)} else line(a, a, a, a)
) in f(15);

face = circle(300,200,200);
tri = triangle(300,50,250,100,350,100);
face_components = let eyes = fun(centerx: int | centerx >= 160):
        (r: shape | (left(r)==centerx-160 and width(r)==320) and (top(r)==200 and height(r)==40)) -> (
    {$rect(centerx-160,200,80,40),rect(centerx+80,200,80,40)}
) in {$eyes(300),tri};

squareshape = let rec squares = fun(a: int | a > 0, x: int | x > 0):
        (r: shape | (left(r)==x and top(r)==x) and (width(r)==a and height(r)==a)) -> (
    let ret = rect(x, x, a, a) in
    if a > 10 then {ret, squares(a - 10, x)} else ret
) in squares(100, 300);

{face, face_components};
squareshape;