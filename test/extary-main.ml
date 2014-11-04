let rec refer n =
  if n < 0 then 4
    else if n > 3 then -4
      else ary.(n)
in
  print_char (refer (-1) + refer 1)
(* expected: output 'h' (104) *)

