let rec write_ppm_header _ =
  ( 
  (* P6 (Binary, PPM) format *)
  (* size is fixed (256 * 256) *)
    let rec p256 x =
       print_char 50; print_char 53; print_char 54 (* 256 *)
    in
    print_char 80; (* 'P' *)
    print_char (48 + 6); (* 48 = '0' *)
    print_char 10;
    p256 ();
    print_char 32;
    p256 ();
    print_char 32;
    print_char 50; print_char 53; print_char 53; (* 255 *)
    print_char 10

  )
in
let size = 256 in
let rec emit r g b = print_char r; print_char g; print_char b in
let rec write_content _ =
  let rec wc_sub x y = if x >= 256 then () else
    if y >= 256 then wc_sub (x + 1) 0
    else (if int_of_float (atan (float_of_int (y - 128) *. 0.7) *. 9.) <>  - x + 128 then emit 255 255 255 else emit 0 0 0; wc_sub x (y+1))
  in wc_sub 0 0
in
write_ppm_header ();
write_content ()

