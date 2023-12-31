open Common

let process_input =
  List.to_seq >> Seq.map (String.to_seq >> Array.of_seq) >> Array.of_seq

(* used for splitters *)
type axis = Horiz | Vert

let is_parallel dir ax =
  match (dir, ax) with
  | N, Vert | S, Vert | E, Horiz | W, Horiz -> true
  | _ -> false

(* used for reflectors *)
type diag = NW_SE | NE_SW

let refl_dir corner heading =
  (* reflects the direction specified by heading according to
     the diagonal defined by corner *)
  match (corner, heading) with
  | NW_SE, N -> W
  | NW_SE, E -> S
  | NW_SE, S -> E
  | NW_SE, W -> N
  | NE_SW, N -> E
  | NE_SW, E -> N
  | NE_SW, S -> W
  | NE_SW, W -> S

type mirror = NoMir | Splitter of axis | Reflector of diag

(* node type for a graph representing the grid *)
type node = coord * direction

let compare_node (c1, d1) (c2, d2) =
  (* we have a quite a few of these, huh *)
  match compare_coord c1 c2 with 0 -> compare_dir d1 d2 | x -> x

let mir_of_coord grid c =
  (* parses the kind of mirror from the input grid given coordinate c *)
  if not @@ in_grid grid c then NoMir
  else
    match grid_get grid c with
    | '/' -> Reflector NE_SW
    | '\\' -> Reflector NW_SE
    | '-' -> Splitter Horiz
    | '|' -> Splitter Vert
    | _ -> NoMir

let ray_penetrates mir heading =
  (* whether a ray penetrates a given mirror (e.g., parallel to splitter) *)
  match mir with
  | NoMir -> true
  | Reflector _ -> false
  | Splitter ax -> is_parallel heading ax

(* dummy node representing the light having left the grid *)
let sink_coord = (Int.min_int, Int.min_int)

let find_next_mirror grid dir c =
  ray_along dir c |> Seq.drop 1
  |> Seq.find (fun c2 ->
         (not (in_grid grid c2))
         || (not @@ ray_penetrates (mir_of_coord grid c2) dir))
  |> Option.get

let parse_mir_and_edges grid c =
  (* generates a sequence of graph edges (n1, n2, cost) for a given mirror
     at position c in grid *)
  let mir = mir_of_coord grid c in
  (* possible directions that light can exit this mirror *)
  let in_out_dirs =
    match mir with
    | NoMir -> failwith "should never get here"
    | Reflector diag ->
        [| N; E; S; W |]
        |> Array.map (fun indir -> (indir, refl_dir diag indir))
    | Splitter Vert -> [| (E, N); (E, S); (W, N); (W, S) |]
    | Splitter Horiz -> [| (N, E); (N, W); (S, E); (S, W) |]
  in
  in_out_dirs |> Array.to_seq
  |> Seq.map (fun (indir, outdir) ->
         let n1 = (c, indir) in
         let c2 = find_next_mirror grid outdir c in
         if in_grid grid c2 then (n1, (c2, outdir), manh_dist c c2)
         else (n1, (sink_coord, outdir), manh_dist c c2 - 1))

module G = Graphs.Make (struct
  type t = node
  type data_t = unit

  let default_data = ()
  let compare = compare_node
end)

let generate_start_edges grid =
  (* generate nodes around the edges of the grid, and edges from them to
     the first mirrors that the light encounters *)
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let start_edge dir c =
    let start_node = (c, dir) in
    let c2 = find_next_mirror grid dir c in
    if not (in_grid grid c2) then
      (start_node, (sink_coord, dir), manh_dist c c2 - 1)
    else (start_node, (c2, dir), manh_dist c c2)
  in
  let left = ray_along S (0, -1) |> Seq.take h |> Seq.map (start_edge E) in
  let right = ray_along S (0, w) |> Seq.take h |> Seq.map (start_edge W) in
  let top = ray_along E (-1, 0) |> Seq.take w |> Seq.map (start_edge S) in
  let bottom = ray_along E (h, 0) |> Seq.take w |> Seq.map (start_edge S) in
  [ left; right; top; bottom ] |> List.to_seq |> Seq.concat

let generate_graph grid : G.t =
  (* finally, after all this effort, generate the graph from the edges *)
  generate_points_on_grid grid
  |> Seq.filter (fun c -> String.contains "/\\-|" @@ grid_get grid c)
  |> Seq.flat_map @@ parse_mir_and_edges grid
  |> Seq.append @@ generate_start_edges grid
  |> G.of_seq_edge_costs

module CoordS = Set.Make (struct
  type t = coord

  let compare = compare_coord
end)

let traverse_graph g start =
  (* depth-first traversal of the graph starting from start, accumulating all
     edges traveled along the way *)
  let rec dfs path seen n1 =
    G.neighbors g n1
    |> Seq.fold_left
         (fun (path, seen) (n2, cost) ->
           let path' = List.cons (n1, n2, cost) path in
           if G.NS.mem n2 seen then (path', seen) else dfs path' seen n2)
         (path, seen |> G.NS.add n1)
  in
  dfs [] G.NS.empty start |> fst |> List.rev

let traverse_graph_arr h w g start =
  (* depth-first traversal of the graph starting from start, accumulating all
     edges traveled along the way *)
  let get mem ((row, col), dir) =
    mem.((((row * w) + col) * 4) + int_of_dir dir)
  in
  let set mem ((row, col), dir) =
    mem.((((row * w) + col) * 4) + int_of_dir dir) <- true;
    mem
  in
  let rec dfs path mem n1 =
    G.neighbors g n1
    |> Seq.fold_left
         (fun (path, mem) (n2, cost) ->
           let path' = List.cons (n1, n2, cost) path in
           if get mem n2 then (path', mem) else dfs path' mem n2)
         (path, set mem n1)
  in
  dfs [] (Array.make (w * h * 4) false) start |> fst |> List.rev

(* generates a sequence of coordinates generated by moving in direction dir n times *)
let steps_along dir n = ray_along dir >> Seq.drop 1 >> Seq.take n

let dir_of (r1, c1) (r2, c2) =
  match compare r1 r2 with
  | -1 -> E
  | 1 -> W
  | 0 -> (
      match compare c1 c2 with -1 -> S | 1 -> N | _ -> failwith "equal dirs")
  | _ -> failwith "unreachable"

let coords_covered traverser g start =
  (* collect all coordinates traversed by following edges using a set
     to prevent potential double counting *)
  traverser g start
  |> List.fold_left
       (fun set ((c1, _), (_, d2), cost) ->
         steps_along d2 cost c1 |> CoordS.of_seq |> CoordS.union set)
       CoordS.empty

let part_1 inp =
  let start_node : node = ((0, -1), E) in
  let grid = process_input inp in
  let graph = grid |> generate_graph in
  coords_covered traverse_graph graph start_node |> CoordS.cardinal

let part_2 inp =
  let grid = process_input inp in
  let graph = generate_graph grid in
  generate_start_edges grid
  |> Seq.map (fun (n1, _, _) -> n1)
  |> Seq.map (coords_covered traverse_graph graph >> CoordS.cardinal)
  |> Seq.fold_left Int.max 0
