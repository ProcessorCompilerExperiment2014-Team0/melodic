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

let rec get_tp nn ov =
  if nn < 12
  then lsr tp_table.(nn) ov
  else get_tp (nn-12) (ov+1)
in

let rec note_on c nn vel =
  let tp = get_tp nn 0 in
  write_chip (c*2) tp;               (* LSB *)
  write_chip (c*2+1) (lsr tp 8);     (* MSB *)
  write_chip (c+8) ((lsr vel 4) + 8) (* set volume *)
in

let rec note_off c =
  write_chip (c+8) 0            (* set volume 0 *)
in

let rec main c0 =
  let msg = read_midi () in
  let state =
    if msg = 144
    then                        (* 144 = 0x90 : ch0 note on *)
      let nn = read_midi () in  (* note number *)
      let vel = read_midi () in (* velocity *)
      let _ = note_off 0 in
      let _ = note_on 0 nn vel in
      nn
    else if msg = 128
    then                        (* 128 = 0x80 : ch0 note off *)
      let nn = read_midi () in  (* note number *)
      let _ = read_midi () in   (* velocity *)
      if nn = c0
      then
        let _ = note_off 0 in
        -1
      else c0
    else c0
  in main state
in

let _ = main (-1)

in 0
