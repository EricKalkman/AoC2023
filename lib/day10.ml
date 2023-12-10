open Common
open Parsing

let line_parser = expect_set "|-LJ7F.S" |> at_least_one >=> skip_whitespace

type heading = N | E | S | W

let advance_coord hd (row, col) =
  match hd with
  | N -> (row - 1, col)
  | S -> (row + 1, col)
  | E -> (row, col + 1)
  | W -> (row, col - 1)

let determine_mouse_tile row col tiles =
  (* This is annoying. I feel like it should be shorter *)
  (* ocamlfmt also blue this up from like 10 lines to 44 *)
  let h, w = (Array.length tiles, Array.length tiles.(0)) in
  let from_north = String.contains "|7F" in
  let from_south = String.contains "|LJ" in
  let from_east = String.contains "-J7" in
  let from_west = String.contains "-LF" in
  if
    row > 0
    && row < h - 1
    && from_north tiles.(row - 1).(col)
    && from_south tiles.(row + 1).(col)
  then '|'
  else if
    row > 0 && col > 0
    && from_north tiles.(row - 1).(col)
    && from_west tiles.(row).(col - 1)
  then 'J'
  else if
    row > 0
    && col < w - 1
    && from_north tiles.(row - 1).(col)
    && from_east tiles.(row).(col + 1)
  then 'L'
  else if
    row < h - 1
    && col > 0
    && from_south tiles.(row + 1).(col)
    && from_west tiles.(row).(col - 1)
  then '7'
  else if
    row < h - 1
    && col < w - 1
    && from_south tiles.(row + 1).(col)
    && from_east tiles.(row).(col + 1)
  then 'F'
  else if
    col > 0
    && col < w - 1
    && from_west tiles.(row).(col - 1)
    && from_east tiles.(row).(col + 1)
  then '-'
  else failwith "Could not determine tile of mouse"

let process_input inp =
  let map =
    List.to_seq inp
    |> Seq.map (fun line -> run_string_parser line_parser line)
    |> Seq.map (fun pline ->
           unwrap_result pline |> List.to_seq |> Seq.map unwrap_char
           |> Array.of_seq)
    |> Array.of_seq
  in
  let mouse_row =
    Array.find_index (fun row -> Array.mem 'S' row) map |> Option.get
  in
  let mouse_col =
    Array.find_index (fun c -> c == 'S') map.(mouse_row) |> Option.get
  in
  let mouse_tile = determine_mouse_tile mouse_row mouse_col map in
  map.(mouse_row).(mouse_col) <- mouse_tile;
  ((mouse_row, mouse_col), map)

let next_coord row col from t =
  match t with
  | '-' -> if from == W then (row, col + 1, W) else (row, col - 1, E)
  | '|' -> if from == N then (row + 1, col, N) else (row - 1, col, S)
  | 'L' -> if from == N then (row, col + 1, W) else (row - 1, col, S)
  | 'J' -> if from == N then (row, col - 1, E) else (row - 1, col, S)
  | '7' -> if from == S then (row, col - 1, E) else (row + 1, col, N)
  | 'F' -> if from == S then (row, col + 1, W) else (row + 1, col, N)
  | _ -> failwith "unimplemented"

let cw_from_dir c =
  match c with
  | 'L' -> E
  | 'J' -> N
  | '7' -> W
  | 'F' -> S
  (* these next two could be either clockwise or ccw *)
  | '|' -> S
  | '-' -> W
  | _ -> failwith "undefined char"

let traverse (row, col) map =
  let start_from_dir = cw_from_dir map.(row).(col) in
  next_coord row col start_from_dir map.(row).(col)
  |> unfold (fun (cur_row, cur_col, from) ->
         if cur_row == row && cur_col == col then None
         else Some (next_coord cur_row cur_col from map.(cur_row).(cur_col)))
  |> Seq.cons (row, col, start_from_dir)

let part_1 inp =
  let pos, map = process_input inp in
  let path_len = traverse pos map |> Seq.length in
  path_len / 2

module EdgeS = Set.Make (struct
  type t = coord * coord

  let compare (c1, c2) (c3, c4) =
    match compare_coord c1 c3 with 0 -> compare_coord c2 c4 | r -> r
end)

module CoordS = Set.Make (struct
  type t = coord

  let compare = compare_coord
end)

module CM = Map.Make (struct
  type t = coord

  let compare = compare_coord
end)

let part_2 inp =
  let pos, map = process_input inp in
  let path =
    traverse pos map
    |> Seq.map (fun (row, col, _) -> (row, col))
    |> Array.of_seq
  in
  let path_set = Array.to_seq path |> CoordS.of_seq in
  let horiz_edges =
    Array.to_seq path
    (* connect first node to last *)
    |> Seq.cons path.(Array.length path - 1)
    |> pairwise
    |> Seq.filter (fun ((row1, _), (row2, _)) -> row1 == row2)
    (* all edges left to right *)
    |> Seq.map (fun ((r1, c1), (r2, c2)) ->
           if c1 > c2 then ((r2, c2), (r1, c1)) else ((r1, c1), (r2, c2)))
    |> EdgeS.of_seq
  in
  (* The algorithm for determining whether a point lies within an arbitrary
     closed shape is to draw a line from that point to a known point outside
     the shape and count how many times that line crosses the shape's boundary.
     If that count is odd, then the point lies inside the shape. Otherwise it
     is outside *)
  let rec calc_crossings (row, col) mem =
    (* Calculates how many times a line from (row, col) to (-1, col) crosses a horizontal
       edge defined by the path. Memoized with Map mem *)
    if row < 0 then (0, mem)
    else
      match CM.find_opt (row, col) mem with
      | Some x -> (x, mem) (* we already calculated this point, return it *)
      | None ->
          (* calculate the number of crossings starting from the point north of this point *)
          let north_crossings, mem = calc_crossings (row - 1, col) mem in
          let res =
            (* if there is an edge directly above the current point *)
            if EdgeS.mem ((row - 1, col), (row - 1, col + 1)) horiz_edges then
              (* increment number of crossings *)
              1 + north_crossings
            else north_crossings
          in
          (res, CM.add (row, col) res mem)
  in
  let coords =
    (* generate all row, col points on the map *)
    generate_grid_points 0 (Array.length map.(0) - 2) 0 (Array.length map - 2)
    (* remove all points on the path *)
    |> Seq.filter (fun c -> not (CoordS.mem c path_set))
  in
  let crossings =
    Seq.fold_left (fun mem c -> snd @@ calc_crossings c mem) CM.empty coords
  in
  coords
  |> Seq.map (fun c -> (c, CM.find c crossings))
  |> Seq.filter (fun (_, n) -> n mod 2 == 1)
  |> Seq.length
