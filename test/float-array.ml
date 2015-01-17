let ary = Array.create 4 14.0 in
let rec sum x y = if x >= 4 then y
  else sum (x + 1) (ary.(x) +. y) in
print_int (int_of_float (sum 0 0.0))

