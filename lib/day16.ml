open Common

let process_input =
  List.to_seq >> Seq.map (String.to_seq >> Array.of_seq) >> Array.of_seq

(* used for splitters *)
type axis = Horiz | Vert

(* comparison for use in Maps and Sets *)
let compare_axis a1 a2 =
  match (a1, a2) with
  | Horiz, Vert -> -1
  | Vert, Horiz -> 1
  | _ -> 0

let is_parallel dir ax =
  match (dir, ax) with
  | N, Vert | S, Vert | E, Horiz | W, Horiz -> true
  | _ -> false

(* used for reflectors *)
type diag = NW_SE | NE_SW

let compare_diag d1 d2 =
  match (d1, d2) with
  | NW_SE, NW_SE -> 0
  | NE_SW, NE_SW -> 0
  | NW_SE, NE_SW -> -1
  | NE_SW, NW_SE -> 1

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

let compare_mirror m1 m2 =
  (* helper compare function for use with sets, maps (i.e., the Graph module) *)
  match (m1, m2) with
  | NoMir, NoMir -> 0
  | NoMir, _ -> -1
  | _, NoMir -> 1
  | Splitter ax1, Splitter ax2 -> compare_axis ax1 ax2
  | Reflector d1, Reflector d2 -> compare_diag d1 d2
  | Splitter _, Reflector _ -> -1
  | _ -> 1

(* used to indicate whether a node represents light _entering_ the
   mirror or _exiting_ the mirror. Need to prevent reflections in
   pathfinding *)
type flowdir = In | Out

let compare_flowdir a b =
  match (a, b) with In, Out -> -1 | Out, In -> 1 | _ -> 0

(* node type for a graph representing the grid *)
type node = coord * mirror * direction * flowdir

let compare_node (c1, m1, d1, f1) (c2, m2, d2, f2) =
  (* we have a quite a few of these, huh *)
  match compare_coord c1 c2 with
  | 0 -> (
      match compare_mirror m1 m2 with
      | 0 -> (
          match compare_dir d1 d2 with 0 -> compare_flowdir f1 f2 | x -> x)
      | x -> x)
  | x -> x

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
let sink_node = ((Int.min_int, Int.min_int), NoMir, N, In)

let parse_mir_and_edges grid c =
  (* generates a sequence of graph edges (n1, n2, cost) for a given mirror
     at position c in grid *)
  let mir = mir_of_coord grid c in
  (* possible directions that light can exit this mirror *)
  let out_dirs =
    match mir with
    | Reflector _ -> [ N; E; S; W ]
    | Splitter Vert -> [ N; S ]
    | _ -> [ E; W ]
  in
  (* changing orientation at a mirror is represented in the graph as a transition
     of cost 0 from (c, mir, dir1, In) to (c, mir, dir2, Out) *)
  let self_edges =
    match mir with
    | NoMir -> failwith "should never get here"
    | Reflector diag ->
        out_dirs |> List.to_seq
        |> Seq.map (fun d ->
               ((c, mir, d, In), (c, mir, refl_dir diag d, Out), 0))
    | Splitter Horiz ->
        [
          ((c, mir, N, In), (c, mir, W, Out), 0);
          ((c, mir, N, In), (c, mir, E, Out), 0);
          ((c, mir, S, In), (c, mir, W, Out), 0);
          ((c, mir, S, In), (c, mir, E, Out), 0);
        ]
        |> List.to_seq
    | Splitter Vert ->
        [
          ((c, mir, E, In), (c, mir, N, Out), 0);
          ((c, mir, E, In), (c, mir, S, Out), 0);
          ((c, mir, W, In), (c, mir, N, Out), 0);
          ((c, mir, W, In), (c, mir, S, Out), 0);
        ]
        |> List.to_seq
  in
  out_dirs |> List.to_seq
  (* find all neighbor coordinates *)
  |> Seq.map (fun dir ->
         ( dir,
           ray_along dir c |> Seq.drop 1
           |> Seq.find (fun c2 ->
                  (not (in_grid grid c2))
                  || (not @@ ray_penetrates (mir_of_coord grid c2) dir))
           |> Option.get ))
  (* create edges out of those neighbor coordinates *)
  |> Seq.map (fun (outdir, c2) ->
         let n1 = (c, mir, outdir, Out) in
         let m2 = mir_of_coord grid c2 in
         match m2 with
         (* if the ray goes off the map, don't count the step off the map *)
         | NoMir -> (n1, sink_node, manh_dist c c2 - 1)
         | _ ->
             let n2 = (c2, m2, outdir, In) in
             (n1, n2, manh_dist c c2))
  |> Seq.append self_edges

module G = Graphs.Make (struct
  type t = node
  type data_t = unit

  let default_data = ()
  let compare = compare_node
end)

let generate_start_edges grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let start_edge dir c =
    let start_node = (c, NoMir, dir, Out) in
    let c2 =
      ray_along dir c |> Seq.drop 1
      |> Seq.find (fun c2 ->
             (not (in_grid grid c2))
             || (not @@ ray_penetrates (mir_of_coord grid c2) dir))
      |> Option.get
    in
    if not (in_grid grid c2) then (start_node, sink_node, manh_dist c c2 - 1)
    else (start_node, (c2, mir_of_coord grid c2, dir, In), manh_dist c c2)
  in
  let left = ray_along S (0, -1) |> Seq.take h |> Seq.map (start_edge E) in
  let right = ray_along S (0, w) |> Seq.take h |> Seq.map (start_edge W) in
  let top = ray_along E (-1, 0) |> Seq.take w |> Seq.map (start_edge S) in
  let bottom = ray_along E (h, 0) |> Seq.take w |> Seq.map (start_edge S) in
  [ left; right; top; bottom ] |> List.to_seq |> Seq.concat

let generate_graph grid : G.t =
  generate_points_on_grid grid
  |> Seq.filter (fun c -> String.contains "/\\-|" @@ grid_get grid c)
  |> Seq.flat_map @@ parse_mir_and_edges grid
  |> Seq.append (generate_start_edges grid)
  |> G.of_seq_edge_costs

let steps_along dir n = ray_along dir >> Seq.drop 1 >> Seq.take n

module CoordS = Set.Make (struct
  type t = coord

  let compare = compare_coord
end)

let traverse_graph g start =
  let rec dfs path seen n1 =
    Seq.fold_left
      (fun (path, seen) (n2, cost) ->
        let path' = List.cons (n1, n2, cost) path in
        if G.NS.mem n2 seen then (path', seen) else dfs path' seen n2)
      (path, seen |> G.NS.add n1)
      (G.neighbors g n1)
  in
  dfs [] G.NS.empty start |> fst |> List.rev

let coords_covered g start =
  traverse_graph g start
  |> List.fold_left
       (fun set ((c1, _, d1, _), _, cost) ->
         steps_along d1 cost c1 |> CoordS.of_seq |> CoordS.union set)
       CoordS.empty

let part_1 inp =
  let start_node : node = ((0, -1), NoMir, E, Out) in
  let graph = process_input inp |> generate_graph in
  coords_covered graph start_node |> CoordS.cardinal

let part_2 inp =
  let grid = process_input inp in
  let graph = generate_graph grid in
  generate_start_edges grid
  |> Seq.map (fun (n1, _, _) -> n1)
  |> Seq.map (coords_covered graph >> CoordS.cardinal)
  |> Seq.fold_left Int.max 0
