let read_other_file fname =
  let lines = ref [] in
  let chan = open_in fname in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let read_input_file fname =
  let fname = "inputs/" ^ fname in
  read_other_file fname

let read_intact_input_file fname = read_input_file fname |> String.concat "\n"
let char_to_str c = String.make 1 c
let sum = Seq.fold_left ( + ) 0
let sum_list = List.fold_left ( + ) 0

(*let rec any p lst =
    match lst with
    | [] -> false
    | x :: xs -> if p x then true else any p xs*)

let ( >> ) f g x = g (f x)

let rec any p sq =
  match Seq.uncons sq with
  | None -> false
  | Some (x, xs) -> if p x then true else any p xs

(*let rec all p lst =
    match lst with
    | [] -> true
    | x :: xs -> if p x then all p xs else false*)
let rec all p sq =
  match Seq.uncons sq with
  | None -> true
  | Some (x, xs) -> if p x then all p xs else false

let compose f g x = f (g x)
let in_range x a b = x >= a && x <= b

let make_range_of_step start stop step =
  let rec aux acc stop' =
    if compare start stop' == -compare start stop then acc
    else aux (Seq.cons stop' acc) (stop' - step)
  in
  aux Seq.empty stop

let make_range lo hi = make_range_of_step lo hi 1
let id x = x

module CustomMap (M : Map.S) = struct
  include M

  let update_unsafe key fn mp =
    M.update key
      (fun x_opt ->
        match x_opt with
        | None -> failwith "update_unsafe"
        | Some x -> Some (fn x))
      mp
end

let update_unsafe mupdate key fn mp =
  mupdate key
    (fun x_opt ->
      match x_opt with
      | None -> failwith "update_unsafe"
      | Some x -> Some (fn x))
    mp

let rec groups_of_n n sq =
  if Seq.is_empty sq then Seq.empty
  else Seq.cons (Seq.take n sq) @@ groups_of_n n (Seq.drop n sq)

type coord = int * int
type direction = N | E | S | W

let neg dir = match dir with N -> S | S -> N | E -> W | W -> E

let compare_coord (x1, y1) (x2, y2) =
  match compare x1 x2 with 0 -> compare y1 y2 | r -> r

let int_of_dir d = match d with N -> 0 | E -> 1 | S -> 2 | W -> 3
let compare_dir d1 d2 = compare (int_of_dir d1) (int_of_dir d2)
let min_coord a b = if compare a b <= 0 then a else b

type rotdir = Rot0 | Rot90 | Rot180 | Rot270

let rotate_dir rot dir =
  match rot with
  | Rot0 -> [| N; E; S; W |].(int_of_dir dir)
  | Rot90 -> [| E; S; W; N |].(int_of_dir dir)
  | Rot180 -> [| S; W; N; E |].(int_of_dir dir)
  | Rot270 -> [| W; N; E; S |].(int_of_dir dir)

let curvature dir1 dir2 =
  match dir1 with
  | N -> [| Rot0; Rot90; Rot180; Rot270 |].(int_of_dir dir2)
  | E -> [| Rot270; Rot0; Rot90; Rot180 |].(int_of_dir dir2)
  | S -> [| Rot180; Rot270; Rot0; Rot90 |].(int_of_dir dir2)
  | W -> [| Rot90; Rot180; Rot270; Rot0 |].(int_of_dir dir2)

let coord_neigh4 (row, col) =
  [| (row, col + 1); (row, col - 1); (row + 1, col); (row - 1, col) |]
  |> Array.to_seq

let coord_neigh4_list (row, col) =
  [ (row, col + 1); (row, col - 1); (row + 1, col); (row - 1, col) ]

let coord_in_box rowlo rowhi collo colhi (row, col) =
  in_range row rowlo rowhi && in_range col collo colhi

let in_grid grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  coord_in_box 0 (h - 1) 0 (w - 1)

let grid_neigh4 grid = coord_neigh4 >> Seq.filter (in_grid grid)
let grid_neigh4_list grid = coord_neigh4_list >> List.filter (in_grid grid)

let translate dir n (row, col) =
  match dir with
  | N -> (row - n, col)
  | E -> (row, col + n)
  | S -> (row + n, col)
  | W -> (row, col - n)

let unfold gen start =
  let rec unfold' cur () =
    match gen cur with None -> Seq.Nil | Some x -> Seq.Cons (cur, unfold' x)
  in
  unfold' start

let ray_along dir = unfold (translate dir 1 >> Option.some)
let grid_get grid (row, col) = grid.(row).(col)

let rec pairwise seq =
  match Seq.uncons seq with
  | None -> Seq.empty
  | Some (x, xs) -> (
      match Seq.uncons xs with
      | None -> Seq.empty
      | Some (y, _) -> fun () -> Seq.Cons ((x, y), pairwise xs))

let generate_grid_points x1 x2 y1 y2 =
  Seq.flat_map
    (fun x -> Seq.map (fun y -> (x, y)) (make_range y1 y2))
    (make_range x1 x2)

let generate_points_on_grid grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  generate_grid_points 0 (h - 1) 0 (w - 1)

let last_row g = Array.length g - 1
let last_col g = Array.length g.(0) - 1
let manh_dist (r1, c1) (r2, c2) = Int.abs (r1 - r2) + Int.abs (c1 - c2)

let rec map_pairs fn sq =
  match Seq.uncons sq with
  | None -> Seq.empty
  | Some (x, xs) -> Seq.append (Seq.map (fun y -> fn x y) xs) @@ map_pairs fn xs

let rec product seq1 seq2 =
  match Seq.uncons seq1 with
  | None -> Seq.empty
  | Some (x, xs) ->
      Seq.append (seq2 |> Seq.map (fun y -> (x, y))) (product xs seq2)

let rec map_pairs_list fn lst =
  match lst with
  | [] -> Seq.empty
  | x :: xs ->
      Seq.append (Seq.map (fun y -> fn x y) (List.to_seq xs))
      @@ map_pairs_list fn xs

let rec remove_pred p lst =
  match lst with
  | [] -> lst
  | y :: ys -> if p y then ys else y :: remove_pred p ys

let rec update_or_emplace_back p x lst =
  match lst with
  | [] -> [ x ]
  | y :: ys -> if p y then x :: ys else y :: update_or_emplace_back p x ys
