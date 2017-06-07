face = circle(300,200,200);
tri = triangle(300,50,250,100,350,100);

face_components = let eyes = fun(centerx: int | centerx >= 160):
        (r: shape | (left(r)==centerx-160 and width(r)==320) and (top(r)==200 and height(r)==40)) -> (
    {$rect(centerx-160,200,80,40),rect(centerx+80,200,80,40)}
) in {$eyes(300),tri};


{face, face_components};
