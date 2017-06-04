open Expr
open Graphics

exception Error

let draw shape =
  Graphics.open_graph " 640x480";
  let rec draw_graphics = function
    | SShape(shape_list) -> List.iter draw_graphics shape_list
    | SRect(SInt(l), SInt(t), SInt(w), SInt(h)) ->
      Graphics.draw_rect l t w h
    | SLine(SInt(p1x), SInt(p1y), SInt(p2x), SInt(p2y)) ->
      Graphics.moveto p1x p1y;
      Graphics.lineto p2x p2y;
    | STriangle(SInt(p1x), SInt(p1y), SInt(p2x), SInt(p2y), SInt(p3x), SInt(p3y)) ->
      Graphics.moveto p1x p1y;
      Graphics.lineto p2x p2y;
      Graphics.lineto p3x p3y
    | SCircle(SInt(cx), SInt(cy), SInt(r)) ->
      Graphics.draw_circle cx cy r
    | _ -> raise Error
  in
  draw_graphics shape
