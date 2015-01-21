let ary = Array.create 3 1.0 in
let rec init _ = 
  ary.(1) <- 2.0;
  ary.(2) <- 3.0 in
init ();
print_int (int_of_float (ary.(0) +. ary.(1) +. ary.(2)))

