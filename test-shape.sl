r = rect(300,200,200,200);
tri = triangle(300,50,250,100,350,100);

let as = {r, tri} in as: shape | left(as) >= 100 and top(as) >= 0;

