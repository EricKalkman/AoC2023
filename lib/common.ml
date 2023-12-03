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
