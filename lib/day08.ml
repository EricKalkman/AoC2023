open Parsing

let triplet_parser = any1 |> repeat 3 |> group >=> stringify_top

let node_parser =
  triplet_parser >=> skip_string " = " >=> skip_char '(' >=> triplet_parser
  >=> skip_string ", " >=> triplet_parser >=> skip_char ')' >=> skip_whitespace
  |> group

let input_parser =
  expect_set "LR" |> at_least_one |> group >=> repeat 2 skip_nl
  >=> (node_parser |> at_least_one)

(* Node map; takes a string and maps it to left and right options (a pair) *)
module NM = Map.Make (String)

let process_input inp =
  let parsed = run_string_parser input_parser inp |> unwrap_result in
  let directions =
    (* translates the list of L/R directions into fst and snd, which will access
       a tuple containing the left and right options of this node *)
    List.hd parsed |> unwrap_group |> List.to_seq
    |> Seq.map (fun c ->
           match unwrap_char c with
           | 'L' -> fst
           | 'R' -> snd
           | _ -> failwith "invalid direction")
  in
  let nodes =
    (* map of node name -> (left node, right node) *)
    List.tl parsed |> List.to_seq
    |> Seq.map (fun g ->
           match unwrap_group g with
           | [ PString src; PString left; PString right ] -> (src, (left, right))
           | _ -> failwith "invalid data format")
    |> NM.of_seq
  in
  (directions, nodes)

let traverse m start stopcond fin_dirseq =
  (* recursively traverse map m from the starting point start until running
     stopcond on the current node returns true. fin_dirseq is a Seq of directions
     that this function will automatically repeat as necessary.

     returns the path taken from start until the ending condition as a string list *)
  let rec traverse_aux path dirseq =
    (* auxiliary recursive traversal *)
    let cur = List.hd path in
    if stopcond cur then path
    else
      let next_dir = Seq.uncons dirseq |> Option.get |> fst in
      traverse_aux
        (List.cons (NM.find cur m |> next_dir) path)
        (Seq.drop 1 dirseq)
  in
  traverse_aux [ start ] (fin_dirseq |> Seq.repeat |> Seq.concat)

let part_1 inp =
  let dirseq, m = process_input inp in
  traverse m "AAA" (( = ) "ZZZ") dirseq |> List.length |> pred

let rec gcd a b =
  if a < b then gcd b a else if b == 0 then a else gcd b (a mod b)

let lcm a b = a * b / gcd a b

let part_2 inp =
  let dirseq, m = process_input inp in
  (* find nodes that end with A *)
  let srcs =
    NM.to_seq m
    |> Seq.filter (fun (src, _) -> String.ends_with ~suffix:"A" src)
    |> Seq.map fst |> List.of_seq
  in
  (* Traverse from each starting node until one ending in Z is encountered *)
  (* too lazy to implement a faster graph type or memoize, so... *)
  Parmap.parmap
    (fun src ->
      traverse m src (String.ends_with ~suffix:"Z") dirseq
      |> List.length |> pred)
    (Parmap.L srcs)
  (* calculate the period *)
  |> List.fold_left lcm 1
