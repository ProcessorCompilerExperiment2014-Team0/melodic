let rec f x = (x + 1, float_of_int x +. 3.0, x + 2, 4.0) in
let ary = Array.create 1 f in
let (y, z, v, w) = ary.(0) 8 in
print_int (y + v + int_of_float z + int_of_float w)

