open Common
open Parsing

let expect_color =
  expect_int >=> skip_whitespace
  >=> expect_option [ "red"; "blue"; "green" ]
  |> group

let expect_grab = expect_list expect_color (expect_string ", ") |> group
let expect_game_data = expect_list expect_grab (expect_string "; ")

let line_parser =
  skip_string "Game " >=> expect_int >=> skip_string ": " >=> expect_game_data

type ballcount = { count : int; color : string }
type round = ballcount list
type game = { id : int; rounds : round list }

let process_round roundgroup : round =
  roundgroup |> unwrap_group
  |> List.map (fun bc ->
         match group_to_pair bc with
         | Int n, PString color -> { count = n; color }
         | _ -> failwith "Unrecognized schema")

let parse_input lines =
  List.to_seq lines
  |> Seq.map (run_string_parser line_parser)
  |> Seq.map (fun raw_line ->
         let line = unwrap_result raw_line in
         {
           id = List.hd line |> unwrap_int;
           rounds = List.tl line |> List.map process_round;
         })
  |> List.of_seq

module ColorMap = Map.Make (String)

let is_game_possible n_balls g =
  all
    (fun round ->
      all
        (fun bc -> ColorMap.find bc.color n_balls >= bc.count)
        (List.to_seq round))
    (List.to_seq g.rounds)

let part_1_maxes =
  [ ("red", 12); ("green", 13); ("blue", 14) ] |> ColorMap.of_list

let part_1 lines =
  parse_input lines |> List.to_seq
  |> Seq.filter (is_game_possible part_1_maxes)
  |> Seq.map (fun g -> g.id)
  |> Seq.fold_left ( + ) 0

let min_cubes game =
  let fold_pull counts bc =
    counts
    |> ColorMap.update bc.color (fun count ->
           match count with
           | None -> Some bc.count
           | Some c -> Some (Int.max c bc.count))
  in
  let start_counts =
    [ ("red", 0); ("green", 0); ("blue", 0) ] |> ColorMap.of_list
  in
  game.rounds
  |> List.fold_left
       (fun counts round -> List.fold_left fold_pull counts round)
       start_counts

let game_power game =
  min_cubes game |> ColorMap.to_seq |> Seq.map snd |> Seq.fold_left ( * ) 1

let part_2 lines =
  parse_input lines |> List.to_seq |> Seq.map game_power
  |> Seq.fold_left ( + ) 0
