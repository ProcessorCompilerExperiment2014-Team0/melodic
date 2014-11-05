let rec atan_sub x =
  x *. (1.0 /. 1. -. x *. x *. (1.0 /. 3. -. x *. x *. (1.0 /. 5. -. x *. x *. (1.0 /. 7. -. x *. x *. (1.0 /. 9. -. x *. x *. (1.0 /. 11. -. x *. x *. (1.0 /. 13. -. x *. x *. (1.0 /. 15. -. x *. x *. (1.0 /. 17. -. x *. x *. (1.0 /. 19. -. x *. x *. (1.0 /. 21. -. x *. x *. (1.0 /. 23.)))))))))))) in
(* let rec atan_sub x = 0.0 in *)
let rec atan x = 
  let pi = 3.1415926535898 in
  if fisneg x then -. atan (-. x)
  else if fless x 0.41421356 then atan_sub x
  else if fless x 1.0 then pi /. 4.0 -. atan_sub ((1.0 -. x) /. (1.0 +. x))
  else if fless x 2.41421356 then pi /. 4.0 +. atan_sub ((x -. 1.) /. (x +. 1.0))
  else pi /. 2.0 -. atan_sub (1.0 /. x) in
print_int (int_of_float (6000000. *. atan (sqrt 3.0)))



