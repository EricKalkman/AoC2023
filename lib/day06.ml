open Parsing

let input_parser =
  skip_string "Time:" >=> skip_whitespace
  >=> (expect_list expect_int expect_whitespace |> group)
  >=> skip_string "Distance:" >=> skip_whitespace
  >=> (expect_list expect_int expect_whitespace |> group)

let process_input inp =
  (* returns a list of (time, best distance) pairs from the input *)
  let lst =
    run_string_parser input_parser inp
    |> unwrap_result
    |> List.map (fun g -> unwrap_group g |> List.map unwrap_int)
  in
  List.map2 (fun a b -> (a, b)) (List.hd lst) (List.nth lst 1)

let accel = 1 (* m/s *)
let accelf = float_of_int accel

let run_race tr tb =
  (* run the race with total time tr and button time tb *)
  let v0 = accel * tb in
  v0 * (tr - tb)

(* no one will ever know what this function is truly for *)
let square f = f *. f
let float_eq_with_tol tol a b = Float.abs (b -. a) < tol
let float_eq = float_eq_with_tol 1e-7

let greater_interval (race_time, best_dist) =
  (* finds the interval of possible integer button hold times over which the boat will
     traver a strictly farther distance than best_dist *)
  let race_timef, best_distf =
    (float_of_int race_time, float_of_int best_dist)
  in
  let desc = square (accelf *. race_timef) -. (4.0 *. accelf *. best_distf) in
  let sqrt_desc = Float.sqrt desc in
  let lof = ((accelf *. race_timef) -. sqrt_desc) /. (2.0 *. accelf) in
  let hif = ((accelf *. race_timef) +. sqrt_desc) /. (2.0 *. accelf) in
  (* There is a tricky edge case here to do with when the bounds of the result
     interval are exact integers. As a rule, floating point operations are not
     exact, and as a result, this can be a difficult check in floating point land.
     The solution applied here is to do fuzzy equality check with a given threshold
     comparing to the rounded value. Do we actually encounter this edge case in the puzzle
     input?
     Who knows! *)
  ( (if float_eq lof (Float.round lof) then Float.round lof +. 1.0
     else Float.ceil lof)
    |> int_of_float,
    (if float_eq hif (Float.round hif) then Float.round hif -. 1.0
     else Float.floor hif)
    |> int_of_float )

let part_1 inp =
  process_input inp |> List.to_seq |> Seq.map greater_interval
  (* convert range to number of integers spanned by that range *)
  |> Seq.map (fun (a, b) -> b - a + 1)
  |> Seq.fold_left ( * ) 1

let part_2_parser =
  (* parse numbers as individual digits instead of as ints *)
  skip_string "Time:" >=> skip_whitespace
  >=> (expect_list (at_least_one expect_digit) skip_whitespace |> group)
  >=> skip_string "Distance:" >=> skip_whitespace
  >=> (expect_list (at_least_one expect_digit) skip_whitespace |> group)

let process_part2_input inp =
  let collapse_string lst =
    List.to_seq lst |> Seq.map unwrap_char |> String.of_seq
  in
  match run_string_parser part_2_parser inp |> unwrap_result with
  | [ Group time_lst; Group dst_lst ] ->
      ( time_lst |> collapse_string |> int_of_string,
        dst_lst |> collapse_string |> int_of_string )
  | _ -> failwith "bad data"

let part_2 inp =
  let a, b = process_part2_input inp |> greater_interval in
  (* convert range to number of integers spanned by that range *)
  b - a + 1
