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

(*let rec any p lst =
    match lst with
    | [] -> false
    | x :: xs -> if p x then true else any p xs*)

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

let make_range lo hi =
  let rec aux acc hi =
    if hi < lo then acc else aux (Seq.cons hi acc) (hi - 1)
  in
  aux Seq.empty hi

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

let rec groups_of_n n sq =
  if Seq.is_empty sq then Seq.empty
  else Seq.cons (Seq.take n sq) @@ groups_of_n n (Seq.drop n sq)

type coord = int * int

let compare_coord (x1, y1) (x2, y2) =
  match compare x1 x2 with 0 -> compare y1 y2 | r -> r

let min_coord a b = if compare a b <= 0 then a else b

let unfold gen start =
  let rec unfold' cur () =
    match gen cur with None -> Seq.Nil | Some x -> Seq.Cons (cur, unfold' x)
  in
  unfold' start

let rec pairwise seq =
  match Seq.uncons seq with
  | None -> Seq.empty
  | Some (x, xs) -> (
      match Seq.uncons xs with
      | None -> Seq.empty
      | Some (y, _) -> fun () -> Seq.Cons ((x, y), pairwise xs))
