open Common

let test_inp = read_input_file "day16.test"
let real_inp = read_input_file "day16.inp"

let process_input =
  List.to_seq >> Seq.map (String.to_seq >> Array.of_seq) >> Array.of_seq

type node = direction * coord

let compare_node (d1, c1) (d2, c2) =
  match compare_dir d1 d2 with 0 -> compare_coord c1 c2 | x -> x

module CS = Set.Make (struct
  type t = coord

  let compare = compare_coord
end)

module NS = Set.Make (struct
  type t = node

  let compare = compare_node
end)

let traverse grid heading c =
  let get_neighbors h c =
    match grid_get grid c with
    | '.' -> [| (h, translate h 1 c) |]
    | '-' -> (
        match h with
        | E | W -> [| (h, translate h 1 c) |]
        | _ -> [| (E, translate E 1 c); (W, translate W 1 c) |])
    | '|' -> (
        match h with
        | N | S -> [| (h, translate h 1 c) |]
        | _ -> [| (N, translate N 1 c); (S, translate S 1 c) |])
    | '/' -> (
        match h with
        | N -> [| (E, translate E 1 c) |]
        | E -> [| (N, translate N 1 c) |]
        | S -> [| (W, translate W 1 c) |]
        | W -> [| (S, translate S 1 c) |])
    | '\\' -> (
        match h with
        | N -> [| (W, translate W 1 c) |]
        | E -> [| (S, translate S 1 c) |]
        | S -> [| (E, translate E 1 c) |]
        | W -> [| (N, translate N 1 c) |])
    | _ -> failwith "undef"
  in
  let rec dfs coords seen h1 c1 =
    get_neighbors h1 c1 |> Array.to_seq
    |> Seq.filter (snd >> in_grid grid)
    |> Seq.fold_left
         (fun (coords, seen) (h2, c2) ->
           if NS.mem (h2, c2) seen then (coords, seen)
           else dfs coords seen h2 c2)
         (coords |> CS.add c1, seen |> NS.add (h1, c1))
  in
  dfs CS.empty NS.empty heading c |> fst

let part_1 inp =
  let grid = process_input inp in
  let start_heading = E in
  let start_pos = (0, 0) in
  traverse grid start_heading start_pos |> CS.cardinal

let generate_starting_nodes grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let left = ray_along S (0, 0) |> Seq.take h |> Seq.map (fun c -> (E, c)) in
  let right =
    ray_along S (0, w - 1) |> Seq.take h |> Seq.map (fun c -> (W, c))
  in
  let top = ray_along E (0, 0) |> Seq.take w |> Seq.map (fun c -> (S, c)) in
  let bottom =
    ray_along E (h - 1, 0) |> Seq.take w |> Seq.map (fun c -> (N, c))
  in
  [| left; right; top; bottom |] |> Array.to_seq |> Seq.concat

let part_2 inp =
  let grid = process_input inp in
  generate_starting_nodes grid
  |> Seq.map (fun (h, c) -> traverse grid h c)
  |> Seq.map CS.cardinal |> Seq.fold_left Int.max 0
