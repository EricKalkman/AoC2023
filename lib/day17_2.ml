open Common

let process_input lines =
  List.to_seq lines
  |> Seq.map
       (String.to_seq
       >> Seq.map (String.make 1 >> int_of_string)
       >> Array.of_seq)
  |> Array.of_seq

(* heading, current position; elided number of steps by modifying the neighbors function
   to generate the reachable neighbors directly *)
type node = direction * coord

let compare_node (d1, c1) (d2, c2) =
  match compare_coord c1 c2 with 0 -> compare_dir d1 d2 | x -> x

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

let neighbors cutter grid (d, c) =
  [ N; E; S; W ] |> List.to_seq
  |> Seq.filter (fun dir -> dir != neg d && dir != d)
  |> Seq.flat_map (fun dir ->
         Seq.unfold
           (fun (cost, c2) ->
             (* accumulate heat cost as well as coordinate along dir *)
             let next_coord = translate dir 1 c2 in
             if in_grid grid next_coord then
               let edge_cost = cost + grid_get grid next_coord in
               (* return the node as well as the accumulated cost *)
               Some (((dir, next_coord), edge_cost), (edge_cost, next_coord))
             else None)
           (0, c)
         (* cutter used to trim the line of nodes generated; e.g., Seq.take 3 for part 1
            to get at most 3 nodes in this direction at which point to turn *)
         |> cutter)

let dijkstra grid neighfunc start stopcond =
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
        |> Seq.fold_left
             (fun (costs, prevs) n_and_cost ->
               let n2, edge_cost = n_and_cost in
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
  dijkstra' (CostS.of_list [ (0, (E, start)); (0, (S, start)) ]) NM.empty

let part_1 inp =
  let grid = process_input inp in
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  dijkstra grid
    (neighbors (Seq.drop 3))
    (0, 0)
    (fun (_, c) -> compare_coord c (h - 1, w - 1) == 0)
  |> fst

let part_2 inp =
  let grid = process_input inp in
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  dijkstra grid
    (neighbors (Seq.drop 3 >> Seq.take 7))
    (0, 0)
    (fun (_, c) -> compare_coord c (h - 1, w - 1) == 0)
  |> fst
