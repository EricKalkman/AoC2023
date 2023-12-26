open Common
open Parsing

let line_parser =
  string_until_char ':' >=> skip_string ": "
  >=> expect_list
        (expect_letter |> at_least_one |> group >=> stringify_top)
        skip_whitespace

module S = Set.Make (String)
module M = Map.Make (String)

let add_to_table p q edges =
  edges
  |> M.update p (function
       | None -> Some (S.singleton q)
       | Some set -> Some (S.add q set))
  |> M.update q (function
       | None -> Some (S.singleton p)
       | Some set -> Some (S.add p set))

let process_input inp =
  List.to_seq inp
  |> Seq.map (fun line ->
         match run_string_parser line_parser line |> unwrap_result with
         | PString src :: snks -> (src, List.map unwrap_ps snks)
         | _ -> failwith "oops")
  |> Seq.fold_left
       (fun edges (p, qs) ->
         qs
         |> List.fold_left
              (fun edges q -> edges |> add_to_table p q |> add_to_table q p)
              edges)
       M.empty

let solve edges =
  (* Essentially, assuming we start at one of the vertices in one of the two
     principal subgraphs of the input, we cut out the most highly connected
     portions of the subgraph. This gradually decreases the connectivity between
     the current subgraph and its complement until it is only 3 edges. There is
     only one such partition with 3 connecting edges per the problem statement *)
  let rec solve' verts =
    (* number of connections of v outside of subgraph defined by verts *)
    let outside_conn v = S.diff (M.find v edges) verts |> S.cardinal in
    let cons =
      S.to_seq verts |> Seq.map (fun v -> (v, outside_conn v)) |> List.of_seq
    in
    (* verts represents one of the two components of the graph connected by
       only 3 edges, so if verts only has 3 connecting edges to its complement,
       then we have succeeded *)
    if cons |> List.map snd |> sum_list == 3 || S.is_empty verts then verts
    else
      (* find the next vertex v in verts that is most highly connected to the
         other component *)
      let mx, _ =
        cons
        |> List.fold_left
             (fun (mv, mc) (v, c) -> if c > mc then (v, c) else (mv, mc))
             ("?", -1)
      in
      solve' (S.remove mx verts)
  in
  (* This nonsense below is all because if the algorithm starts in the wrong place (i.e.,
     on one of the vertices on the edge of the cut) AND moves in the wrong direction (i.e.,
     across the cut), then vertices from opposite components of the graph both end up in the
     same component (the complement of `verts`). Obviously, this causes the algorithm to fail,
     as it does with the test input.
     The algorithm generally works fine assuming that the above conditions don't happen, so the
     easiest way to ensure correctness is to just start from a different node if it fails. *)
  edges |> M.to_seq |> Seq.map fst
  |> Seq.map (fun to_remove ->
         solve' (M.to_seq edges |> Seq.map fst |> S.of_seq |> S.remove to_remove))
  |> Seq.drop_while (fun set -> S.is_empty set)
  |> Seq.uncons |> Option.get |> fst

let part_1 inp =
  let edges = process_input inp in
  let n_comp1 = edges |> solve |> S.cardinal in
  let total_nodes = M.cardinal edges in
  n_comp1 * (total_nodes - n_comp1)
