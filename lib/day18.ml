open Common
open Parsing

type instruction = {
  dir : direction;
  steps : int;
  real_dir : direction;
  real_steps : int;
}

let line_parser =
  expect_set "UDLR" >=> skip_whitespace >=> expect_int >=> skip_whitespace
  >=> skip_string "(#"
  >=> (expect_hexdigit |> repeat 5 |> group >=> stringify_top >=> expect_int)
  >=> skip_char ')'

let process_input inp =
  let dir_of_char = function 'U' -> N | 'D' -> S | 'R' -> E | _ -> W in
  let get_real hexstr realdir =
    ( (match realdir with 0 -> E | 1 -> S | 2 -> W | _ -> N),
      int_of_string ("0x" ^ hexstr) )
  in
  inp
  |> List.map (fun line ->
         match run_string_parser line_parser line |> unwrap_result with
         | [ Char dir; Int steps; PString hexstr; Int realdir ] ->
             let real_dir, real_steps = get_real hexstr realdir in
             { dir = dir_of_char dir; steps; real_dir; real_steps }
         | _ -> failwith "invalid inp format")

(* helper functions for working with orientation *)
let int_of_rot = function Rot90 -> 1 | _ -> -1
let rot_of_int = function 1 -> Rot90 | _ -> Rot270

let get_turns dirs =
  (* curvature gives the positive rotation that converts dir a into dir b *)
  (* This function takes adjacent pairs of directions and converts them to
     an infinite sequence of rotations ("turns") *)
  dirs |> Seq.cycle |> pairwise |> Seq.map (fun (a, b) -> curvature a b)

let overall_curvature turns len =
  (* Takes the turns that define a loop and the length of the loop and determines
     whether the loop is overall clockwise (Rot90) or counter-clockwise (Rot270)
     by converting all turns to their integer values (either -1 or 1), sums them,
     and uses whether that value is positive or negative to give the loop rotation *)
  turns |> Seq.map int_of_rot |> Seq.take len |> Seq.fold_left ( - ) 0
  |> compare 0 |> rot_of_int

let trace_path insts dirfunc stepfunc =
  (* takes a list of insts, an inst -> direction, and an inst -> number of steps *)
  let len = List.length insts in
  let turns = List.to_seq insts |> Seq.map dirfunc |> get_turns in
  let loop_dir = overall_curvature turns len in
  (* the first n-1 elements of turns are dropped so that the second element
     is the turn that happens at the end of executing the zipped instruction *)
  Seq.zip (turns |> pairwise |> Seq.drop (len - 1)) (List.to_seq insts)
  (* scan: the Seq function I didn't know I needed *)
  |> Seq.scan
       (fun coord ((prev_turn, next_turn), next_inst) ->
         (* if the two turns are in the same direction, we are either convex or concave
            and need to adjust the path length appropriately to stay on the outside edge
            of the polygon. If convex, then add 1 to the length, if concave, then subtract 1 *)
         let fudge =
           ([ prev_turn; next_turn ] |> List.map int_of_rot
          |> List.fold_left ( + ) 0)
           / 2 * int_of_rot loop_dir
         in
         translate (dirfunc next_inst) (stepfunc next_inst + fudge) coord)
       (0, 0)
  |> Seq.drop 1

let part dirfunc stepfunc inp =
  let insts = process_input inp in
  let len = List.length insts in
  (* turns out if you sum the cross products of the coordinates of all adjacent points
     of a polygon, you get twice the signed area of the polygon *)
  let twice_area =
    trace_path insts dirfunc stepfunc
    |> Seq.cycle |> pairwise |> Seq.take len
    |> Seq.fold_left
         (fun acc ((r1, c1), (r2, c2)) -> acc + (r1 * c2) - (c1 * r2))
         0
  in
  twice_area / 2 |> Int.abs

let part_1 = part (fun inst -> inst.dir) (fun inst -> inst.steps)
let part_2 = part (fun inst -> inst.real_dir) (fun inst -> inst.real_steps)
