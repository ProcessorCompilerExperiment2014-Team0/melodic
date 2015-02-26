let rec mandelbrot_at a b =
  let rec loop cnt x y =
    if cnt <= 0 then
      255
    else if (x *. x) +. (y *. y) >= 4.0 then
      0
    else
      loop (cnt - 1) ((x *. x) -. (y *. y) +. a) (2.0 *. x *. y +. b)
  in
  loop 200 0.0 0.0
in let a = -. 1.0 +. 14.0 /. 32.0
in let b = -. 2.0 +. 72.0 /. 32.0
in print_int (mandelbrot_at a b) (* 4262 = 3 * 1416 + 1, 1416  = 96 * 14 + 72, -1 + 14 / 32 = -11/16, -2 + 72 / 32 = 0.25 *)
