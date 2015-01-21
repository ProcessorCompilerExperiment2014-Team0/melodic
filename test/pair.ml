let rec f x = (x + 1, x + 2) in
let ary = Array.create 1 f in
let (y, z) = ary.(0) 8 in
print_int (y + z)

