open Common

let process_input lines =
  let map =
    List.to_seq lines
    |> Seq.map (fun s -> String.to_seq s |> Array.of_seq)
    |> Array.of_seq
  in
  let galaxies =
    Seq.filter (fun (row, col) -> map.(row).(col) == '#')
    @@ generate_grid_points 0 (last_row map) 0 (last_col map)
    |> List.of_seq
  in
  let void_rows =
    Seq.filter
      (fun row ->
        make_range 0 @@ last_col map |> all (fun col -> map.(row).(col) == '.'))
      (make_range 0 @@ last_row map)
    |> List.of_seq
  in
  let void_cols =
    Seq.filter
      (fun col ->
        make_range 0 @@ last_row map |> all (fun row -> map.(row).(col) == '.'))
      (make_range 0 @@ last_col map)
    |> List.of_seq
  in
  (galaxies, void_rows, void_cols)

let rec expand_space_along_dim axes get set factor galaxies =
  match Seq.uncons axes with
  | None -> galaxies
  | Some (ax, axs) ->
      let dilated_g =
        galaxies
        |> Seq.map (fun c ->
               if get c <= ax then c else set (fun x -> x + factor - 1) c)
      in
      let dilated_axes = axs |> Seq.map (fun a' -> a' + (factor - 1)) in
      expand_space_along_dim dilated_axes get set factor dilated_g

let expand_space void_rows void_cols factor galaxies =
  galaxies
  |> expand_space_along_dim void_rows fst
       (fun f (row, col) -> (f row, col))
       factor
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
