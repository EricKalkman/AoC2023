open Parsing
open Common

let brick_parser = expect_list expect_int (expect_char ',')
let line_parser = brick_parser >=> skip_char '~' >=> brick_parser

type coord3 = { x : int; y : int; z : int }

let compare_coord3 { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  match compare x1 x2 with
  | 0 -> ( match compare y1 y2 with 0 -> compare z1 z2 | a -> a)
  | a -> a

type brick = { lo : coord3; hi : coord3 }

let process_line inp =
  match run_string_parser line_parser inp |> unwrap_result with
  | [ Int x1; Int y1; Int z1; Int x2; Int y2; Int z2 ] ->
      {
        lo = { x = Int.min x1 x2; y = Int.min y1 y2; z = Int.min z1 z2 };
        hi =
          {
            x = Int.max x1 x2 + 1;
            y = Int.max y1 y2 + 1;
            z = Int.max z1 z2 + 1;
          };
      }
  | _ -> failwith "invalid line format"

let process_input = List.map process_line

let compare_brick b1 b2 =
  match compare_coord3 b1.lo b2.lo with
  | 0 -> compare_coord3 b1.hi b2.hi
  | x -> x

let xy_coords { lo; hi } =
  Seq.product (make_range lo.x (hi.x - 1)) (make_range lo.y (hi.y - 1))

let ground =
  {
    lo = { x = Int.min_int; y = Int.min_int; z = 0 };
    hi = { x = Int.max_int; y = Int.max_int; z = 1 };
  }

let compare_brick_top_face b1 b2 =
  match -compare b1.hi.z b2.hi.z with 0 -> -compare_brick b1 b2 | x -> x

let bricks_intersect b1 b2 =
  if compare_brick b1 ground == 0 || compare_brick b2 ground == 0 then true
  else
    CoordS.inter
      (b1 |> xy_coords |> CoordS.of_seq)
      (b2 |> xy_coords |> CoordS.of_seq)
    |> CoordS.is_empty |> not

module PQ = Set.Make (struct
  type t = brick

  let compare = compare_brick_top_face
end)

module PM = Map.Make (struct
  type t = brick

  let compare = compare_brick
end)

let stack bricks =
  let stack' stacked b =
    let top_brick =
      stacked |> PQ.to_seq
      |> Seq.find (fun b2 -> b2.hi.z <= b.lo.z && bricks_intersect b b2)
      |> Option.get
    in
    let diff = b.lo.z - top_brick.hi.z in
    let translated =
      {
        lo = { x = b.lo.x; y = b.lo.y; z = top_brick.hi.z };
        hi = { x = b.hi.x; y = b.hi.y; z = b.hi.z - diff };
      }
    in
    PQ.add translated stacked
  in
  let sorted = bricks |> List.sort (fun b1 b2 -> compare b1.lo.z b2.lo.z) in
  sorted |> List.fold_left stack' (PQ.singleton ground)

let get_touching tanjcon bricks =
  bricks |> PQ.to_seq
  |> Seq.map (fun b1 ->
         ( b1,
           bricks
           (*|> List.filter (fun b2 -> b.lo.z == b2.hi.z && bricks_intersect b b2)*)
           |> PQ.filter (fun b2 -> tanjcon b1 b2 && bricks_intersect b1 b2) ))
  |> PM.of_seq

let gen_graph bricks =
  let resting_on = get_touching (fun b1 b2 -> b1.lo.z == b2.hi.z) bricks in
  let supporting = get_touching (fun b1 b2 -> b1.hi.z == b2.lo.z) bricks in
  (resting_on, supporting)

let part_1 inp =
  let bricks = process_input inp |> stack in
  let resting_on, supporting = process_input inp |> stack |> gen_graph in
  bricks |> PQ.to_seq
  |> Seq.filter (fun b ->
         match PM.find_opt b supporting with
         | None -> true
         | Some supported ->
             PQ.to_seq supported
             |> all (fun above ->
                    resting_on |> PM.find above |> PQ.cardinal > 1))
  |> Seq.length

let num_supporting resting_on supporting b =
  let rec num_supporting' resting_on = function
    | [] -> 0
    | b1 :: bs ->
        let supported = supporting |> PM.find b1 in
        let resting_on =
          PQ.fold
            (fun above resting_on ->
              resting_on
              |> update_unsafe PM.update above (fun set -> set |> PQ.remove b1))
            supported resting_on
        in
        let unsupported =
          supported
          |> PQ.filter (fun b2 -> resting_on |> PM.find b2 |> PQ.is_empty)
        in
        PQ.cardinal unsupported
        + num_supporting' resting_on (List.append bs (PQ.to_list unsupported))
  in
  num_supporting' resting_on [ b ]

let part_2 inp =
  let bricks = process_input inp |> stack in
  let resting_on, supporting = process_input inp |> stack |> gen_graph in
  bricks |> PQ.to_seq
  |> Seq.filter (fun b -> compare_brick b ground != 0)
  |> Seq.map (num_supporting resting_on supporting)
  |> sum
