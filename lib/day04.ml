open Common
open Parsing

let tokenize_line =
  skip_string "Card " >=> skip_whitespace >=> expect_int >=> skip_char ':'
  >=> skip_whitespace
  >=> (expect_list expect_int expect_whitespace |> group)
  >=> skip_whitespace >=> skip_char '|' >=> skip_whitespace
  >=> (expect_list expect_int expect_whitespace |> group)

let tokenize_input lines =
  List.to_seq lines |> Seq.map (run_string_parser tokenize_line)

type game = { id : int; winning_nos : int list; player_nos : int list }

let process_input lines =
  lines |> tokenize_input
  |> Seq.map (fun rawline ->
         let line = unwrap_result rawline in
         {
           id = List.hd line |> unwrap_int;
           winning_nos = List.nth line 1 |> unwrap_group |> List.map unwrap_int;
           player_nos = List.nth line 2 |> unwrap_group |> List.map unwrap_int;
         })

module IntS = Set.Make (Int)

let n_wins g =
  let score_nums =
    IntS.inter
      (List.to_seq g.winning_nos |> IntS.of_seq)
      (List.to_seq g.player_nos |> IntS.of_seq)
  in
  IntS.cardinal score_nums

let score_game g =
  let wins = n_wins g in
  if wins == 0 then 0
  else make_range 1 wins |> Seq.drop 1 |> Seq.fold_left (fun acc _ -> 2 * acc) 1

let part_1 lines =
  lines |> process_input |> Seq.map score_game |> Seq.fold_left ( + ) 0

module IntM = CustomMap (Map.Make (Int))

let part_2 lines =
  let games = process_input lines in
  let init_map = games |> Seq.map (fun g -> (g.id, 1)) |> IntM.of_seq in
  games
  |> Seq.fold_left
       (fun m g ->
         let wins = n_wins g in
         let n_copies = IntM.find g.id m in
         Seq.fold_left
           (fun m gid -> IntM.update_unsafe gid (fun c -> c + n_copies) m)
           m
           (make_range (g.id + 1) (g.id + wins)))
       init_map
  |> IntM.to_seq |> Seq.map snd |> Seq.fold_left ( + ) 0
