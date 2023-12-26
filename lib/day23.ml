open Common

let process_input lines =
  lines |> List.to_seq
  |> Seq.map (fun str -> str |> String.to_seq |> Array.of_seq)
  |> Array.of_seq

let neighbors ?(ignore_slopes = false) grid c =
  match grid_get grid c with
  | 'v' when not ignore_slopes -> [ translate S 1 c ]
  | '^' when not ignore_slopes -> [ translate N 1 c ]
  | '<' when not ignore_slopes -> [ translate W 1 c ]
  | '>' when not ignore_slopes -> [ translate E 1 c ]
  | '.' | '<' | '>' | 'v' | '^' ->
      grid_neigh4 grid c
      |> Seq.filter (fun c2 ->
             Array.mem (grid_get grid c2) [| '.'; '<'; '>'; '^'; 'v' |])
      |> List.of_seq
  | _ -> []

module CM = Map.Make (struct
  type t = coord

  let compare = compare_coord
end)

module PQ = Set.Make (struct
  type t = coord * int

  let compare (c1, cost1) (c2, cost2) =
    match compare cost1 cost2 with 0 -> compare_coord c1 c2 | x -> x
end)

let dijkstra start neighfunc =
  let rec dijkstra' prevs q prev =
    if PQ.is_empty q then prevs
    else
      let c1, cost1 = PQ.min_elt q in
      let q = PQ.remove (c1, cost1) q in
      let neighbors =
        neighfunc c1
        (* prevent walking backwards *)
        |> List.filter (fun (n, _) -> compare_coord n prev != 0)
        |> List.filter_map (fun (c2, cost_c1c2) ->
               match CM.find_opt c2 prevs with
               | None -> Some (c2, (c1, cost1 + cost_c1c2))
               | Some (_, old_c2_cost) ->
                   if cost1 + cost_c1c2 < old_c2_cost then
                     Some (c2, (c1, cost1 + cost_c1c2))
                   else None)
      in
      dijkstra'
        (CM.add_seq (List.to_seq neighbors) prevs)
        (PQ.add_seq
           (List.to_seq neighbors |> Seq.map (fun (n, (_, c)) -> (n, c)))
           q)
        c1
  in
  dijkstra' (CM.singleton start (start, 0)) (PQ.singleton (start, 0)) start

let part_1 inp =
  let grid = process_input inp in
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let start = (0, 1) in
  let stop = (h - 1, w - 2) in
  let neighfunc = neighbors grid >> List.map (fun c -> (c, -1)) in
  dijkstra start neighfunc |> CM.find stop |> snd |> ( - ) 0

let max_dim = 1000
let hash_coord (row, col) = (max_dim * row) + col

module HCS = Set.Make (Int)
module HCM = Map.Make (Int)

let dfs neighfunc stopcond folder fold0 ret start =
  let rec dfs' seen cost c1 =
    if compare_coord start c1 != 0 && stopcond c1 then ret c1 cost
    else
      neighfunc c1
      |> List.filter (fun (c2, _) -> not @@ HCS.mem (hash_coord c2) seen)
      |> List.map (fun (c2, c1_c2_cost) ->
             dfs' (HCS.add (hash_coord c1) seen) (cost + c1_c2_cost) c2)
      |> List.fold_left folder fold0
  in
  dfs' HCS.empty 0 start

let is_intersection grid c =
  neighbors ~ignore_slopes:true grid c |> List.length > 2

let telescope_graph grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let src = (0, 1) in
  let snk = (h - 1, w - 2) in
  let neighfunc =
    neighbors ~ignore_slopes:true grid >> List.map (fun c -> (c, 1))
  in
  let dfser =
    dfs neighfunc (is_intersection grid)
      (fun lst a -> Seq.append a lst)
      Seq.empty
      (fun c cost -> Seq.return (c, cost))
  in
  let src_edges = dfser src |> List.of_seq in
  let snk_edges = dfser snk in
  let edges =
    generate_points_on_grid grid
    |> Seq.filter (is_intersection grid)
    |> Seq.map (fun c1 -> (hash_coord c1, dfser c1 |> List.of_seq))
    |> HCM.of_seq
    |> HCM.add (hash_coord src) src_edges
  in
  snk_edges
  |> Seq.fold_left
       (fun edges (n, cost) -> HCM.add_to_list (hash_coord n) (snk, cost) edges)
       edges

let print_edge_graph edges =
  edges
  |> CM.iter (fun (row, col) lst ->
         Printf.printf "(%d, %d): " row col;
         lst
         |> List.iter (fun ((r2, c2), cost) ->
                Printf.printf "(%d, %d | %d); " r2 c2 cost);
         print_newline ())

let part_2 inp =
  let grid = process_input inp in
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let start = (0, 1) in
  let stop = (h - 1, w - 2) in
  let telescoped = telescope_graph grid in
  let neighfunc c = HCM.find (hash_coord c) telescoped in
  let stopcond c = compare_coord c stop == 0 in
  dfs neighfunc stopcond max 0 (fun _ c -> c) start
