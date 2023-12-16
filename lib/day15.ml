open Common
open Parsing

let parse_input = String.split_on_char ','

type command = Remove of string | Replace of string * int

let cmd_parser =
  string_until_set "-="
  >=> (expect_char '-' >=>? (expect_char '=' >=> expect_int))

let parse_cmd cmd =
  match run_string_parser cmd_parser cmd |> unwrap_result with
  | [ PString label; Char '-' ] -> Remove label
  | [ PString label; Char '='; Int focal_len ] -> Replace (label, focal_len)
  | _ -> failwith ("Unrecognized format for cmd: " ^ cmd)

let hash_cmd =
  String.to_seq >> Seq.fold_left (fun h c -> (h + int_of_char c) * 17 mod 256) 0

let part_1 inp =
  let cmds = parse_input inp in
  cmds |> List.map hash_cmd |> List.fold_left ( + ) 0

let process_command boxes cmd =
  let remove_lens boxes label =
    let h = hash_cmd label in
    boxes.(h) <- remove_pred (fst >> String.equal label) boxes.(h);
    boxes
  in
  let replace_lens boxes label focal_len =
    let h = hash_cmd label in
    boxes.(h) <-
      update_or_emplace_back (fst >> String.equal label) (label, focal_len) boxes.(h);
    boxes
  in
  match cmd with
  | Remove label -> remove_lens boxes label
  | Replace (label, len) -> replace_lens boxes label len

let part_2 inp =
  let boxes = Array.make 256 [] in
  parse_input inp |> List.map parse_cmd
  |> List.fold_left process_command boxes
  |> Array.mapi (fun boxnum lenses ->
         List.to_seq lenses
         |> Seq.fold_lefti
              (fun sum slot (_, len) -> sum + ((boxnum + 1) * (slot + 1) * len))
              0)
  |> Array.fold_left ( + ) 0
