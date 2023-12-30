open Common

let test_inp = read_input_file "day01.test"
let real_inp = read_input_file "day01.inp"

let digits_of_string str =
  str
  |> String.fold_left
       (fun (dig1, dign) c ->
         if String.contains "0123456789" c then
           match dig1 with
           | None -> (Some c, Some c)
           | Some d -> (Some d, Some c)
         else (dig1, dign))
       (None, None)

let part_1 inp =
  inp |> List.to_seq |> Seq.map digits_of_string
  |> Seq.map (fun (a, b) ->
         Option.(String.make 1 (get a) ^ String.make 1 (get b) |> int_of_string))
  |> sum

let find_in_string str sub =
  let rec find' i =
    if i + String.length sub >= String.length str then None
    else if String.equal (String.sub str i (String.length sub)) sub then Some i
    else find' (i + 1)
  in
  find' 0

let rfind_in_string str sub =
  let rec find' i =
    if i < 0 then None
    else if String.equal (String.sub str i (String.length sub)) sub then Some i
    else find' (i - 1)
  in
  find' (String.length str - String.length sub)

let part_2 inp =
  let all_kinds_of_digits str =
    let find set finder cmp dummy =
      set |> List.to_seq
      |> Seq.mapi (fun i x -> (i, finder x))
      |> Seq.filter (snd >> Option.is_some)
      |> Seq.fold_left
           (fun (mi, mx) (i, ox) ->
             if cmp (Option.get ox) mx then (i, Option.get ox) else (mi, mx))
           (-1, dummy)
    in
    let digits = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] in
    let digit_names =
      [
        "zero";
        "one";
        "two";
        "three";
        "four";
        "five";
        "six";
        "seven";
        "eight";
        "nine";
      ]
    in
    let left_dig, left_dig_pos =
      find digits (fun c -> String.index_opt str c) ( < ) Int.max_int
    in
    let right_dig, right_dig_pos =
      find digits (fun c -> String.rindex_opt str c) ( > ) Int.min_int
    in
    let left_strdig, left_strdig_pos =
      find digit_names (find_in_string str) ( < ) Int.max_int
    in
    let right_strdig, right_strdig_pos =
      find digit_names (rfind_in_string str) ( > ) Int.min_int
    in
    let left = if left_strdig_pos < left_dig_pos then left_strdig else left_dig in
    let right = if right_strdig_pos > right_dig_pos then right_strdig else right_dig in
    left * 10 + right
  in
  inp |> List.to_seq |> Seq.map all_kinds_of_digits |> sum
