open OUnit2
open Expr
open Draw

let test_cases = [
  (*SRect(SInt(10), SInt(10), SInt(50), SInt(50));*)
  SShape([
      SRect(SInt(70), SInt(100), SInt(50), SInt(50));
      SCircle(SInt(100), SInt(100), SInt(20));
      SLine(SInt(10), SInt(10), SInt(50), SInt(50));
      STriangle(SInt(10), SInt(10), SInt(50), SInt(50), SInt(30), SInt(30))
    ])
]

let () =
  List.iter draw test_cases;
  let _ = read_line () in ()
(*Random.self_init ();;
  Graphics.open_graph " 640x480";;

  let rec iterate r x_init i =
  if i = 1 then x_init
  else
    let x = iterate r x_init (i-1) in
    r *. x *. (1.0 -. x);;

  for x = 0 to 639 do
  let r = 4.0 *. (float_of_int x) /. 640.0 in
  for i = 0 to 39 do
    let x_init = Random.float 1.0 in
    let x_final = iterate r x_init 500 in
    let y = int_of_float (x_final *. 480.) in
    Graphics.plot x y
  done
  done;;

  read_line ();;*)
