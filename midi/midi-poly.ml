let tp_table =
  let table = create_array 12 0 in
  table.( 0) <- 15289;
  table.( 1) <- 14431;
  table.( 2) <- 13621;
  table.( 3) <- 12856;
  table.( 4) <- 12135;
  table.( 5) <- 11454;
  table.( 6) <- 10811;
  table.( 7) <- 10204;
  table.( 8) <-  9631;
  table.( 9) <-  9091;
  table.(10) <-  8581;
  table.(11) <-  8099;
  table
in

let state =
  create_array 3 (-1)
in

let rec get_tp nn ov =
  if nn < 12
  then lsr tp_table.(nn) ov
  else get_tp (nn-12) (ov+1)
in

let rec note_on c nn vel =
  let tp = get_tp nn 0 in
  if tp > 4096
  then ()
  else (
    write_chip (c*2) tp;               (* LSB *)
    write_chip (c*2+1) (lsr tp 8);     (* MSB *)
    write_chip (c+8) ((lsr vel 4) + 8)) (* set volume *)
in

let rec note_off c =
  write_chip (c+8) 0            (* set volume 0 *)
in

let rec lookup c nn =
  if c >= 3
  then -1
  else
    if state.(c) = nn
    then c
    else lookup (c+1) nn
in

let rec try_note_off nn =
  let c = lookup 0 nn in
  if c < 0
  then
    ()
  else
    let _ = note_off c in
    state.(c) <- (-1)
in

let rec get_available c =
  if c >= 3
  then 0
  else
    if state.(c) < 0
    then c
    else get_available (c+1)
in

let rec main _ =
  let msg = read_midi () in
  let _ =
    if msg = 144
    then                        (* 144 = 0x90 : ch0 note on *)
      let nn = read_midi () in  (* note number *)
      let vel = read_midi () in (* velocity *)
      if vel = 0
      then
        try_note_off nn
      else
        let c = get_available 0 in
        let _ = note_off c in
        let _ = state.(c) <- nn in
        note_on c nn vel
    else if msg = 128
    then                        (* 128 = 0x80 : ch0 note off *)
      let nn = read_midi () in  (* note number *)
      let _ = read_midi () in   (* velocity *)
      try_note_off nn
    else ()
  in main ()
in

let _ =
  let _ = write_chip 7 56 in
  main ()

in 0
