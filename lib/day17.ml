open Common

let process_input lines =
  List.to_seq lines
  |> Seq.map
       (String.to_seq
       >> Seq.map (String.make 1 >> int_of_string)
       >> Array.of_seq)
  |> Array.of_seq

(* heading, current position, number of steps left *)
type node = direction * coord * int

let compare_node (d1, c1, s1) (d2, c2, s2) =
  match compare_dir d1 d2 with
  | 0 -> ( match compare_coord c1 c2 with 0 -> compare s1 s2 | x -> x)
  | x -> x

type node_and_cost = node * int

module NM = Map.Make (struct
  type t = node

  let compare = compare_node
end)

module CostS = Set.Make (struct
  type t = int * node

  let compare (c1, n1) (c2, n2) =
    match compare c1 c2 with 0 -> compare_node n1 n2 | x -> x
end)

let neighbors_part1 grid (d, c, s) =
  [ N; E; S; W ]
  |> List.filter (fun dir -> dir != neg d && not (s == 0 && dir == d))
  |> List.map (fun dir ->
         (dir, translate dir 1 c, if dir == d then s - 1 else 2))
  |> List.filter (fun (_, c2, _) -> in_grid grid c2)

let dijkstra grid neighfunc start start_s stopcond =
  let neighbors = neighfunc grid in
  let pop_node costs =
    let res = CostS.min_elt costs in
    (res, CostS.remove res costs)
  in
  let rec dijkstra' costs prevs =
    let (n1_cost, n1), costs = pop_node costs in
    if stopcond n1 then (n1_cost, n1)
    else
      let costs, prevs =
        neighbors n1
        |> List.fold_left
             (fun (costs, prevs) n2 ->
               let _, c2, _ = n2 in
               let edge_cost = grid_get grid c2 in
               let new_n2_cost = n1_cost + edge_cost in
               match NM.find_opt n2 prevs with
               | None ->
                   ( costs |> CostS.add (new_n2_cost, n2),
                     NM.add n2 (new_n2_cost, n1) prevs )
               | Some (prev_cost, _) ->
                   if prev_cost <= new_n2_cost then (costs, prevs)
                   else
                     ( CostS.add (new_n2_cost, n2) costs,
                       NM.add n2 (new_n2_cost, n1) prevs ))
             (costs, prevs)
      in
      dijkstra' costs prevs
  in
  dijkstra' (CostS.singleton (0, (E, start, start_s))) NM.empty

let part_1 inp =
  let grid = process_input inp in
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  dijkstra grid neighbors_part1 (0, 0) 3 (fun (_, c, _) ->
      compare_coord c (h - 1, w - 1) == 0)
  |> fst

let neighbors_part2 grid (d, c, s) =
  let start =
    if s > 0 && s < 4 then [ d ]
    else
      [ N; E; S; W ]
      |> List.filter (fun dir -> dir != neg d && not (dir == d && s == 10))
  in
  start
  |> List.map (fun dir ->
         (dir, translate dir 1 c, if dir == d then s + 1 else 1))
  |> List.filter (fun (_, c, _) -> in_grid grid c)

let part_2 inp =
  let grid = process_input inp in
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  dijkstra grid neighbors_part2 (0, 0) 0 (fun (_, c, s) ->
      s >= 4 && compare_coord c (h - 1, w - 1) == 0)
  |> fst
