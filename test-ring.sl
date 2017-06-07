let rec drawcircles = fun(cx: int | cx > 0, cy: int | cy > 0, r: int | (r < cx and r < cy) and r > 0, ringsz: int | ringsz > 0): shape -> (
    let ret = circle(cx, cy, r) in
    if r > ringsz then {ret, drawcircles(cx, cy, r - ringsz, ringsz)} else ret
) in drawcircles(320, 240, 200, 20);
