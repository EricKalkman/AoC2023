module type NodeType = sig
  type t
  type data_t

  val default_data : data_t
  val compare : t -> t -> int
end

module type S = sig
  type nt
  type ntd
  type t

  val empty : t
  val has_node : nt -> t -> bool
  val has_edge : nt -> nt -> t -> bool
  val add_node : nt -> ntd -> t -> t
  val add_edges : (nt * nt) Seq.t -> t -> t
  val add_nodes : (nt * ntd) Seq.t -> t -> t
  val of_seq_edges : (nt * nt) Seq.t -> t
  val of_seq_nodes : (nt * ntd) Seq.t -> t
  val edges_to_seq : t -> (nt * nt) Seq.t
  val neighbors : nt -> t -> nt Seq.t
end

module Make (NT : NodeType) = struct
  (* directed graph, implemented as adjacency list *)
  type nt = NT.t
  type ntd = NT.data_t

  module NM = Map.Make (struct
    type t = NT.t

    let compare = NT.compare
  end)

  module EM = Map.Make (struct
    type t = NT.t

    let compare = NT.compare
  end)

  module ES = Set.Make (struct
    type t = NT.t

    let compare = NT.compare
  end)

  type t = ntd NM.t * ES.t EM.t

  let empty : t = (NM.empty, EM.empty)
  let has_node node (ndata, _) = NM.mem node ndata

  let has_edge n1 n2 (_, edges) =
    match EM.find_opt n1 edges with
    | None -> false
    | Some neighbors -> ES.mem n2 neighbors

  let add_node_data sq (ndata, edges) = (NM.add_seq sq ndata, edges)

  let add_edge n1 n2 (ndata, edges) =
    let try_add_node n ndata =
      NM.update n
        (fun x -> match x with None -> Some NT.default_data | y -> y)
        ndata
    in
    let new_data = ndata |> try_add_node n1 |> try_add_node n2 in
    let new_edges =
      EM.update n1
        (fun neighbors ->
          match neighbors with
          | None -> Some (ES.singleton n2)
          | Some s -> Some (ES.add n2 s))
        edges
    in
    (new_data, new_edges)

  let add_node n data (ndata, edges) = (NM.add n data ndata, edges)
  let add_edges sq g = Seq.fold_left (fun g (n1, n2) -> add_edge n1 n2 g) g sq
  let add_nodes sq g = Seq.fold_left (fun g (n, nd) -> add_node n nd g) g sq
  let of_seq_nodes sq = add_nodes sq (NM.empty, EM.empty)
  let of_seq_edges sq = add_edges sq (NM.empty, EM.empty)

  let edges_to_seq (_, edges) =
    EM.to_seq edges
    |> Seq.flat_map (fun (n1, s) -> ES.to_seq s |> Seq.map (fun n2 -> (n1, n2)))

  let neighbors n (_, edges) =
    match EM.find_opt n edges with None -> Seq.empty | Some s -> ES.to_seq s

  let flood_find src (nodes, edges) =
    let rec dfs n q =
      Seq.fold_left
        (fun q neigh -> if ES.mem neigh q then q else dfs neigh q)
        q
        (neighbors n (nodes, edges))
      |> ES.add n
    in
    dfs src ES.empty |> ES.to_seq
end
