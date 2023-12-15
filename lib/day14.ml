open Common

let process_input lines =
  (* convert each line into a char array *)
  List.to_seq lines
  |> Seq.map (fun line -> String.to_seq line |> Array.of_seq)
  |> Array.of_seq

let count_char c seq =
  (* count the number of instances of char c in a seq *)
  seq |> Seq.fold_left (fun acc d -> if c == d then acc + 1 else acc) 0

let tilt_line indexer setter len =
  (* Takes a string of tiles indexed by indexer and modifiable by calling set and
     moves all rolling stones toward lower indices.
     This is accomplished by converting the line to a string, splitting the string at
     fixed rocks ('#'), moving all 'O's toward the lowest indices, reassembling the
     string, and then finally using the setter to update the line using the string. *)
  (* convert the line to a string *)
  let line = make_range 0 (len - 1) |> Seq.map indexer |> String.of_seq in
  line |> String.split_on_char '#'
  (* for each tract between fixed rocks *)
  |> List.map (fun tract ->
         (* count rollers in tract *)
         let n_rollers = count_char 'O' (tract |> String.to_seq) in
         (* shortcut if there are none; no modification needed *)
         if n_rollers == 0 then tract
         else
           (* create a new string putting all of the rollers at the start *)
           String.make n_rollers 'O'
           ^ String.make (String.length tract - n_rollers) '.')
  (* join the string with '#'s again *)
  |> String.concat "#"
  (* update the line *)
  |> String.iteri (fun i c -> setter i c)

(* getters and setters functions for lines along each cardinal direction *)
let n_getter board col row = board.(row).(col)
let s_getter board col row = board.(Array.length board - row - 1).(col)
let w_getter board row col = board.(row).(col)
let e_getter board row col = board.(row).(Array.length board.(0) - col - 1)
let n_setter board col row c = board.(row).(col) <- c
let s_setter board col row c = board.(Array.length board - row - 1).(col) <- c
let w_setter board row col c = board.(row).(col) <- c

let e_setter board row col c =
  board.(row).(Array.length board.(0) - col - 1) <- c

let tilt_board dir board =
  let getter =
    match dir with
    | N -> n_getter
    | E -> e_getter
    | S -> s_getter
    | W -> w_getter
  in
  let setter =
    match dir with
    | N -> n_setter
    | E -> e_setter
    | S -> s_setter
    | W -> w_setter
  in
  (* length of getter/setter range *)
  let len =
    match dir with
    | N | S -> Array.length board
    | E | W -> Array.length board.(0)
  in
  (* range of the other dimesion, i.e., if dir is N or S then 0 .. width-1,
     else 0 .. height-1 *)
  let other_dim =
    make_range 0
      ((match dir with
       | N | S -> Array.length board.(0)
       | E | W -> Array.length board)
      - 1)
  in
  other_dim
  (* update the board along each line in the dimension *)
  |> Seq.iter (fun d -> tilt_line (getter board d) (setter board d) len);
  board

let calc_load board =
  board |> Array.to_seq
  |> Seq.fold_lefti
       (fun res rowdx row ->
         res + (count_char 'O' (Array.to_seq row) * (Array.length board - rowdx)))
       0

let part_1 inp =
  let board = process_input inp in
  board |> tilt_board N |> calc_load

let do_cycle board =
  board |> tilt_board N |> tilt_board W |> tilt_board S |> tilt_board E

module StrM = Map.Make (String)

let cycle_til_repeat board =
  let rec cycle_til_repeat' board n mem =
    let board_str =
      Array.to_seq board |> Seq.map Array.to_seq |> Seq.concat |> String.of_seq
    in
    match StrM.find_opt board_str mem with
    | Some prev_iter -> (prev_iter, n - prev_iter)
    | None ->
        let mem = StrM.add board_str n mem in
        cycle_til_repeat' (do_cycle board) (n + 1) mem
  in
  cycle_til_repeat' board 0 StrM.empty

let part_2 inp =
  let n_iter = 1_000_000_000 in
  let board = process_input inp in
  let offset, cyc_len = cycle_til_repeat board in
  let target_iter = n_iter - offset in
  let leftover = target_iter mod cyc_len in
  make_range 1 leftover
  |> Seq.fold_left (fun board _ -> do_cycle board) board
  |> calc_load
