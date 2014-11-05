let rec write_ppm_header _ =
  ( 
  (* P6 (Binary, PPM) format *)
  (* size is fixed (128 * 128) *)
    let rec p128 x =
       print_char 49; print_char 50; print_char 56 (* 128 *)
    in
    print_char 80; (* 'P' *)
    print_char (48 + 6); (* 48 = '0' *)
    print_char 10;
    p128 ();
    print_char 32;
    p128 ();
    print_char 32;
    print_char 50; print_char 53; print_char 53; (* 255 *)
    print_char 10

  )
in
let ary = create_array (16384) 0 in
let rec emit r g b = print_char r; print_char g; print_char b in
let rec t128 x y = let r = x * 4 in
  let rr = r + r + r + r in
  let rrr = rr + rr + rr + rr in
  rrr + y + rrr in
let rec write_content ary =
  let rec wc_sub x y = if x >= 128 then () else
    if y >= 128 then wc_sub (x + 1) 0
    else (if int_of_float (sin (float_of_int (y - 64) *. 1.) *. 2.5) <> 64 - x  then emit 255 255 255 else emit 0 0 0; wc_sub x (y+1))
  in wc_sub 0 0
in
write_ppm_header ();
write_content ary

