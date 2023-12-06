open Common
open Parsing

let seed_parser =
  skip_string "seeds: "
  >=> expect_list expect_int (expect_char ' ')
  >=> skip_nl |> group

let table_line_parser =
  expect_nonempty_list expect_int (expect_char ' ')
  >=> (skip_nl >=>? expect_eof) |> group

let table_parser =
  string_until_str "-to-" >=> skip_string "-to-" >=> string_until_char ' '
  >=> skip_string " map:\n"
  >=> (table_line_parser |> at_least_one)
  |> group

let input_parser =
  seed_parser >=> skip_nl >=> expect_list table_parser expect_nl

type node_type = string * int

let compare (s1, n1) (s2, n2) =
  let scmp = String.compare s1 s2 in
  if scmp == 0 then compare n1 n2 else scmp

let process_input inp =
  (* returns an ungodly lambda taking (class name, number) to the linked (class name, number)
     where 'class name' is seed, soil, location, etc.*)
  let tokenized = run_string_parser input_parser inp |> unwrap_result in
  let seeds = List.hd tokenized |> unwrap_group |> List.map unwrap_int in

  (* lambda for checking if the query number is in a custom range for the current map
     (instead of the default option of src num -> dst num) *)
  let gen_range_map dst_num src_num length query_num =
    if in_range query_num src_num (src_num + length - 1) then
      Some (dst_num + (query_num - src_num))
    else None
  in

  let parse_map lst =
    let src = List.hd lst |> unwrap_ps in
    let dst = List.nth lst 1 |> unwrap_ps in
    let num_filter_fn =
      List.to_seq lst |> Seq.drop 2
      |> Seq.fold_left
           (fun acc g ->
             match unwrap_group g with
             | [ Int dst_num; Int src_num; Int len ] -> (
                 fun (* returns a function taking the number in src number in question (assuming matching
                            class) and returning the destination (class, num) pair *)
                       query_num ->
                   match gen_range_map dst_num src_num len query_num with
                   | Some x -> Some (dst, x)
                   | None -> acc query_num)
             | _ -> failwith "map group of improper format")
           (fun query_num -> Some (dst, query_num))
    in

    (* final function; checks that the class name matches, and if so gets the destination number that the
       source maps to *)
    fun (query_name, query_num) ->
      if String.equal query_name src then num_filter_fn query_num else None
  in

  let maps =
    List.map (fun g -> unwrap_group g |> parse_map) (List.tl tokenized)
    |> List.fold_left
         (fun acc m query ->
           match m query with Some x -> Some x | None -> acc query)
         (fun _ -> None)
  in
  (seeds, maps)

let get_location graph seednum =
  let rec tramp (cls, num) =
    match cls with
    | "location" -> num
    | _ -> (
        match graph (cls, num) with
        | None -> failwith ("Could not find location! Last node: " ^ cls)
        | Some node -> tramp node)
  in
  tramp ("seed", seednum)

let part_1 inp =
  let seeds, graph = process_input inp in
  List.to_seq seeds
  |> Seq.map (get_location graph)
  |> Seq.fold_left Int.min Int.max_int

(* complete reimplementation for Part 2 *)
(* We can no longer evaluate travel through the graph by mapping every seed in the range
   across the function we generated for Part 1; there are too many. Instead, we can consider
   only the extrema of the seed ranges. If we encounter a range map that only covers part of
   the seed range, we split the seed range to a part that is transformed and a part that is not.
   However, of those two new ranges, we only consider their extrema as before. Once we arrive at
   the last map (the location map), we simply look for the lowest minimum of any of our ranges *)

type data_range = { lo : int; hi : int }

let process_part2 inp =
  (* process the input into ranges of seeds and mappings of ranges to other ranges *)
  let tokenized = run_string_parser input_parser inp |> unwrap_result in
  let seeds = List.hd tokenized |> unwrap_group |> List.map unwrap_int in
  let seed_ranges =
    List.to_seq seeds |> groups_of_n 2
    |> Seq.map (fun sq ->
           match List.of_seq sq with
           | [ seed_num; len ] -> { lo = seed_num; hi = seed_num + len - 1 }
           | _ -> failwith "invalid schema")
    |> List.of_seq
  in

  let process_table g =
    unwrap_group g |> List.to_seq |> Seq.drop 2
    |> Seq.map (fun line ->
           match unwrap_group line with
           | [ Int dst; Int src; Int len ] ->
               ( { lo = src; hi = src + len - 1 },
                 { lo = dst; hi = dst + len - 1 } )
           | _ -> failwith "invalid schema")
    |> List.of_seq
  in
  (seed_ranges, List.map process_table (List.tl tokenized))

let rec apply_map m num =
  (* transform num according to the mapping m *)
  match m with
  | [] -> num
  | (src, dst) :: m' ->
      if in_range num src.lo src.hi then dst.lo + (num - src.lo)
      else apply_map m' num

let apply_map_to_range m rng =
  (* transform the range rng according to the mapping m *)
  { lo = apply_map m rng.lo; hi = apply_map m rng.hi }

let fragment_range rng1 rng2 =
  (* break up rng1 if it partially intersects with rng2 *)
  (* rng2 is the range of inputs that can be transformed by a mapping *)
  if in_range rng1.lo rng2.lo rng2.hi && rng1.hi > rng2.hi then
    [ { lo = rng1.lo; hi = rng2.hi }; { lo = rng2.hi + 1; hi = rng1.hi } ]
  else if in_range rng1.hi rng2.lo rng2.hi && rng1.lo < rng2.lo then
    [ { lo = rng1.lo; hi = rng2.lo - 1 }; { lo = rng2.lo; hi = rng1.hi } ]
  else [ rng1 ]

let ranges_through_map rngs m =
  (* take a list of ranges and put them through a one-layer mapping *)
  List.fold_left
    (fun rngs (src, _) ->
      rngs |> Seq.flat_map (fun rng -> fragment_range rng src |> List.to_seq))
    rngs m
  |> Seq.map (apply_map_to_range m)

let part_2 inp =
  let seed_ranges, maps = process_part2 inp in
  (* apply all the mappings *)
  List.fold_left ranges_through_map (List.to_seq seed_ranges) maps
  |> Seq.map (fun rng -> rng.lo)
  (* find the minimum lo value of all fragmented ranges *)
  |> Seq.fold_left Int.min Int.max_int
