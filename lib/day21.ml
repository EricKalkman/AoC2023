open Common

let process_input =
  List.to_seq >> Seq.map (String.to_seq >> Array.of_seq) >> Array.of_seq

let start_pos grid =
  grid
  |> Array.find_mapi (fun rowdx row ->
         match row |> Array.find_index (( == ) 'S') with
         | None -> None
         | Some coldx -> Some (rowdx, coldx))
  |> Option.get

module CM = Map.Make (struct
  type t = coord

  let compare = compare_coord
end)

let neighbors grid c =
  grid_neigh4 grid c |> Seq.filter (fun c -> grid_get grid c != '#')

let bfs grid neighfunc start =
  let rec bfs' depths q =
    match q with
    | [] -> depths
    | (depth, cur) :: q ->
        let neighs =
          neighfunc grid cur
          |> Seq.filter (fun n -> not @@ CM.mem n depths)
          |> List.of_seq
        in
        let depth = depth + 1 in
        let depths =
          List.fold_left (fun prevs n -> CM.add n depth prevs) depths neighs
        in
        bfs' depths (List.append q (List.map (fun n -> (depth, n)) neighs))
  in
  bfs' (CM.singleton start 0) [ (0, start) ]

let tiles_flashing_at depth prevs =
  (* once tiles are reachable, every other step they are reachable again (they flash) *)
  (* count up the tiles that are reachable based on whether an odd or even number of
     steps has been taken *)
  CM.to_seq prevs |> Seq.map snd
  |> Seq.filter (fun d -> d <= depth && d mod 2 == depth mod 2)

let part_1 inp =
  let grid = process_input inp in
  let start = start_pos grid in
  bfs grid neighbors start |> tiles_flashing_at 64 |> Seq.length

let part_2 inp =
  (* man I'm not even gonna try to explain this, I'm not even sure that
     it's broadly correct *)
  (* I will say that it assumes that the origin has an unobstructed path to the edge
     of the map, that the origin is centered on the map, and that the map has an odd
     width. I tihnk it also assumes that all corners are reachable in 131 steps from
     the center. *)
  let total_steps = 26501365 in
  let grid = process_input inp in
  let start = start_pos grid in
  let dists = bfs grid neighbors start in
  let width = Array.length grid in
  (* starting position is centered *)
  let half_width = fst start in
  let repeat_radius = (total_steps - half_width) / width in
  (* As we traverse from the center map outwards, the tiled maps alternate
     having their even-distance plots lighting up or their odd-distance ones.
     The last tiled map before the step count has points lit up of a certain
     parity. This is the list of those points in a theoretical fully-filled
     tile. *)
  let points_of_edge_parity =
    Seq.(
      CM.to_seq dists
      |> filter (fun (_, d) -> d mod 2 != repeat_radius mod 2)
      |> List.of_seq)
  in
  let points_of_non_edge_parity =
    Seq.(
      CM.to_seq dists
      |> filter (fun (_, d) -> d mod 2 == repeat_radius mod 2)
      |> List.of_seq)
  in
  let num_points_edge_parity = List.length points_of_edge_parity in
  let num_points_non_edge_parity = List.length points_of_non_edge_parity in
  (* Some tiled maps are not fully covered; the points not covered have a
     distance > half_width *)
  let num_points_edge_corner =
    List.(
      points_of_edge_parity |> filter (fun (_, d) -> d > half_width) |> length)
  in
  (* some tiles are partially covered; these have opposite parity but are otherwise
     symmetrical to num_points_edge_corner *)
  let num_points_non_edge_corner =
    List.(
      points_of_non_edge_parity
      |> filter (fun (_, d) -> d > half_width)
      |> length)
  in
  (* number of fully filled tiles with edge parity *)
  let num_edge_parity = (repeat_radius + 1) * (repeat_radius + 1) in
  let num_non_edge_parity = repeat_radius * repeat_radius in
  (* number of corners of edge parity, i.e., those that we need to cut out *)
  let num_edge_corners = repeat_radius + 1 in
  (* number of corners without edge parity, i.e., those that we need to add in *)
  let num_non_edge_corners = repeat_radius in
  (num_edge_parity * num_points_edge_parity)
  + (num_non_edge_parity * num_points_non_edge_parity)
  - (num_edge_corners * num_points_edge_corner)
  + (num_non_edge_corners * num_points_non_edge_corner)
