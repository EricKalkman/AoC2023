open Common
open Parsing

(* crappy parser-combinator stuff. nothing to see here *)
let grid_parser = expect_list (expect_set ".#" |> at_least_one |> group) skip_nl
let inp_parser = expect_list (grid_parser |> group) skip_nl

(* rok = rock; 3 chars for easy alignment when printing *)
type tile = Ash | Rok

let tile_of_char c = match c with '.' -> Ash | _ -> Rok

let process_input inp =
  (* returns a list of boards (tile array array's) *)
  run_string_parser inp_parser inp
  |> unwrap_result
  |> List.map (fun g ->
         unwrap_group g |> List.to_seq
         |> Seq.map (fun g2 ->
                unwrap_group g2 |> List.to_seq
                |> Seq.map (fun x -> unwrap_char x |> tile_of_char)
                |> Array.of_seq)
         |> Array.of_seq)

(* Begin real code *)
module IntS = Set.Make (Int)

let find_reflections indexer len =
  (* Takes a function that indexes along an axis in which we are trying to
     find reflection, as well as the length of that axis.
     Returns a set (IntS.t) containing all possible reflection points in the axis *)
  let rec find_reflections' acc idx pref suff =
    (* Traverses the list suff, cons'ing the first element of suff onto pref as we go.
       This has the effect of taking the first idx items of a list and reversing them
       as pref, leaving the remainder in suff. Along the way, if pref and suff are
       symmetric (i.e., if the shorter of the two is a prefix of the longer), the split
       point idx is added to the set acc. *)
    match suff with
    | [] ->
        acc
        (* no more suffix to check for splits, return the accumulated reflection idxs *)
    | s :: ss ->
        (* Seq.for_all2 only takes elements until the shorter of its two Seqs are exhausted.
           This if therefore checks if pref is a prefix of suff or vice versa.
           Because pref contains items from the original list in reverse order, equal prefixes
           indicates a reflection *)
        let acc' =
          if Seq.for_all2 ( == ) (List.to_seq pref) (List.to_seq suff) then
            IntS.add idx acc
          else acc
        in
        find_reflections' acc' (idx + 1) (List.cons s pref) ss
  in
  (* Generate a list of each element along the axis *)
  match make_range 0 (len - 1) |> Seq.map indexer |> List.of_seq with
  | x :: xs -> find_reflections' IntS.empty 1 [ x ] xs
  | _ -> failwith "unreachable"

let board_refl board =
  let height = Array.length board in
  let width = Array.length board.(0) in
  (* indexing functions along a specific row *)
  let hidxers = Array.to_seq board |> Seq.map (fun row i -> row.(i)) in
  (* indexing functions along a specific column *)
  let vidxers =
    make_range 0 (width - 1) |> Seq.map (fun coldx i -> board.(i).(coldx))
  in
  let hs =
    hidxers
    (* find the reflections along each row *)
    |> Seq.map (fun indexer -> find_reflections indexer width)
    (* The only reflections valid for the whole board are those that are shared
       among each row. Therefore, intersect the sets from each row to find the
       one in common. *)
    |> Seq.fold_left IntS.inter (make_range 0 (width - 1) |> IntS.of_seq)
  in
  let vs =
    vidxers
    |> Seq.map (fun indexer -> find_reflections indexer height)
    |> Seq.fold_left IntS.inter (make_range 0 (height - 1) |> IntS.of_seq)
  in
  (hs, vs)

let part_1 inp =
  let h, v =
    process_input inp |> List.to_seq |> Seq.map board_refl |> Seq.split
  in
  (* each board should only have at most one horizontal reflection. This mess extracts that
     one value from the set for each board, if it exists, and sums the lot. *)
  (h |> Seq.map IntS.to_seq
  |> Seq.map (fun x -> match Seq.uncons x with None -> 0 | Some (x, _) -> x)
  |> Seq.fold_left ( + ) 0)
  + 100
    * (v |> Seq.map IntS.to_seq
      |> Seq.map (fun x ->
             match Seq.uncons x with None -> 0 | Some (x, _) -> x)
      |> Seq.fold_left ( + ) 0)

let flip_tile t = match t with Rok -> Ash | Ash -> Rok

let map_smudges fn board =
  (* Helper function that smudges each tile on the board (i.e., flips one Ash to Rok
     or one Rok with Ash) and then passes it to fn, collecting the results as a Seq *)
  generate_grid_points 0 (Array.length board - 1) 0 (Array.length board.(0) - 1)
  |> Seq.map (fun (row, col) ->
         board.(row).(col) <- flip_tile board.(row).(col);
         let r = fn board in
         (* board is a ref type (Array), so need to reset it before the next iter *)
         board.(row).(col) <- flip_tile board.(row).(col);
         r)

let part2_refl board =
  let part1_hs, part1_vs = board_refl board in
  let smudged_hs, smudged_vs =
    (* calculate reflections for each smudged permutation of board *)
    board |> map_smudges board_refl
    (* accumulate all possible board reflections *)
    |> Seq.fold_left
         (fun (hs, vs) (h, v) -> (IntS.union hs h, IntS.union vs v))
         (IntS.empty, IntS.empty)
  in
  (* We're told to look for *different* reflection axes from part 1,
     so remove those from consideration *)
  (IntS.diff smudged_hs part1_hs, IntS.diff smudged_vs part1_vs)

let part_2 inp =
  (* Could I avoid code duplication by lifting this and part_1 into another function? Yes.
     Will I? No. *)
  let h, v =
    process_input inp |> List.to_seq |> Seq.map part2_refl |> Seq.split
  in
  (h |> Seq.map IntS.to_seq
  |> Seq.map (fun x -> match Seq.uncons x with None -> 0 | Some (x, _) -> x)
  |> Seq.fold_left ( + ) 0)
  + 100
    * (v |> Seq.map IntS.to_seq
      |> Seq.map (fun x ->
             match Seq.uncons x with None -> 0 | Some (x, _) -> x)
      |> Seq.fold_left ( + ) 0)
