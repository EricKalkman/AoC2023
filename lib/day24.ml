open Common
open Rat_coord3
open Parsing
open Num

type hailstone = { pos : vec3; vel : vec3 }

let parse_line =
  expect_list expect_int (expect_char ',' >=> skip_whitespace)
  >=> skip_whitespace >=> skip_char '@' >=> skip_whitespace
  >=> expect_list expect_int (expect_char ',' >=> skip_whitespace)

let process_input =
  List.map (fun line ->
      match run_string_parser parse_line line |> unwrap_result with
      | [ Int x; Int y; Int z; Int dx; Int dy; Int dz ] ->
          { pos = (Int x, Int y, Int z); vel = (Int dx, Int dy, Int dz) }
      | _ -> invalid_arg "Failed to process input string")

let get_intersection_times h1 h2 =
  let dx, dy, _ = subv h2.pos h1.pos in
  let vx, vy, _ = h1.vel in
  let wx, wy, _ = h2.vel in
  let a, b, c, d = (vx, wx, vy, wy) in
  let dim1, dim2 = (dx, dy) in
  let det = (a */ d) -/ (b */ c) in
  if eq_num det (Int 0) then None
  else
    let det_inv = Int 1 // det in
    let a_inv, b_inv, c_inv, d_inv =
      ( d */ det_inv,
        Int (-1) */ b */ det_inv,
        Int (-1) */ c */ det_inv,
        a */ det_inv )
    in
    let t = (a_inv */ dim1) +/ (b_inv */ dim2) in
    let s = Int (-1) */ ((c_inv */ dim1) +/ (d_inv */ dim2)) in
    Some (t, s)

let eval_traj h t = scale t h.vel |> addv h.pos

let in_test_area lo hi p =
  let x, y, _ = p in
  Num.(ge_num x lo && le_num x hi && ge_num y lo && le_num y hi)

let part_1 inp =
  let part_1_min = Int 200000000000000 in
  let part_1_max = Int 400000000000000 in
  inp |> process_input
  |> map_pairs_list (fun h1 h2 -> (h1, h2, get_intersection_times h1 h2))
  |> Seq.filter_map (fun (h1, _, inter) ->
         match inter with
         | None -> None
         | Some (t, s) when lt_num t (Int 0) || lt_num s (Int 0) -> None
         | Some (t, _) -> Some (eval_traj h1 t))
  |> Seq.filter (in_test_area part_1_min part_1_max)
  |> Seq.length

let reframe hs =
  (* change reference frame to first hailstone. the thrown stone must then pass
     through the origin *)
  let zero = List.hd hs in
  ( zero,
    List.(
      hs |> tl
      |> map (fun h -> { pos = subv h.pos zero.pos; vel = subv h.vel zero.vel }))
  )

let unreframe zero h = { pos = addv h.pos zero.pos; vel = addv h.vel zero.vel }

let calc_plane_normal hs =
  let h = List.hd hs in
  let p0 = eval_traj h (Int 0) in
  let p1 = eval_traj h (Int 1000) in
  let n = cross p0 p1 in
  (n, List.tl hs)

let get_plane_intersection n p0 h = dot (subv p0 h.pos) n // dot h.vel n

let get_v n p0 hs =
  let h1 = List.hd hs in
  let h2 = List.nth hs 1 in
  let int1_t = get_plane_intersection n p0 h1 in
  let int1_p = eval_traj h1 int1_t in
  let int2_t = get_plane_intersection n p0 h2 in
  let int2_p = eval_traj h2 int2_t in
  let dx = subv int2_p int1_p in
  let dt = int2_t -/ int1_t in
  let v = scale_inv dt dx in
  let throw_start =
    eval_traj { pos = int1_p; vel = scale (Int (-1)) v } int1_t
  in
  (throw_start, v)

let get_throw hs =
  let zero, hs = reframe hs in
  let n, hs = calc_plane_normal hs in
  let p, v = get_v n zero3 hs in
  unreframe zero { pos = p; vel = v }

let part_2 inp =
  inp |> process_input |> get_throw |> fun { pos; _ } ->
  vx pos +/ vy pos +/ vz pos
