open Common

let process_input lines =
  let map =
    List.to_seq lines
    |> Seq.map (fun s -> String.to_seq s |> Array.of_seq)
    |> Array.of_seq
  in
  let galaxies =
    (* coordinates of all galaxies *)
    Seq.filter (fun (row, col) -> map.(row).(col) == '#')
    @@ generate_grid_points 0 (last_row map) 0 (last_col map)
    |> List.of_seq
  in
  let void_rows =
    (* row coordinates of all void rows *)
    Seq.filter
      (fun row ->
        make_range 0 @@ last_col map |> all (fun col -> map.(row).(col) == '.'))
      (make_range 0 @@ last_row map)
    |> List.of_seq
  in
  let void_cols =
    (* col coordinates of all void cols *)
    Seq.filter
      (fun col ->
        make_range 0 @@ last_row map |> all (fun row -> map.(row).(col) == '.'))
      (make_range 0 @@ last_col map)
    |> List.of_seq
  in
  (galaxies, void_rows, void_cols)

let rec expand_space_along_dim axes get set factor galaxies =
  (* Modifies the coordinates given by galaxies to include space expansion
     along the axes given in the Seq axes. get takes a coordinate and returns
     the dimensional value (i.e., the row or col of the galaxy depending on
     whether the axes lie east-west or north-south, respectively), while set
     modifies that dimensional value and reincorporates into a coord *)
  (* Axes is assumed to be sorted low to high; thus, the coordinates are
     traversed in ascending order. As axes are encountered, the proper dimensional
     coordinate of all points beyond that axis are increased by the space expansion
     factor *)
  match Seq.uncons axes with
  | None -> galaxies
  | Some (ax, axs) ->
      let dilated_g =
        galaxies
        |> Seq.map (fun c ->
               (* if a galaxy lies beyond the axis, add the expansion factor to the
                  appropriate coordinate *)
               if get c <= ax then c else set (fun x -> x + factor - 1) c)
      in
      (* don't forget to expand the coordinates of the axes as we go *)
      let dilated_axes = axs |> Seq.map (fun a' -> a' + (factor - 1)) in
      expand_space_along_dim dilated_axes get set factor dilated_g

let expand_space void_rows void_cols factor galaxies =
  galaxies
  (* expand the void rows *)
  |> expand_space_along_dim void_rows fst
       (fun f (row, col) -> (f row, col))
       factor
  (* expand the void columns *)
  |> expand_space_along_dim void_cols snd
       (fun f (row, col) -> (row, f col))
       factor

let part factor inp =
  let galaxies, void_rows, void_cols = process_input inp in
  expand_space (void_rows |> List.to_seq) (void_cols |> List.to_seq) factor
    (galaxies |> List.to_seq)
  |> List.of_seq |> map_pairs_list manh_dist |> Seq.fold_left ( + ) 0

let part_1 = part 2
let part_2 = part 1000000
