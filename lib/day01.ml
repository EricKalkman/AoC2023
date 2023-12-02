open Parsing

let get_digits (str : string) =
    String.to_seq str
    |> Seq.filter_map (fun c ->
            match c with
        | '0'..'9' -> Some (Char.code c - Char.code '0')
        | _ -> None)
    |> Array.of_seq

let part_1 lines =
    List.map (fun line ->
        let digits = get_digits line in
        digits.(0) * 10 + digits.(Array.length digits - 1))
        lines
    |> List.fold_left (+) 0

(* How is this the way to reverse strings? *)
let reverse_string str =
    str |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq

let num_words = [
    ("one",   "1");
    ("two",   "2");
    ("three", "3");
    ("four",  "4");
    ("five",  "5");
    ("six",   "6");
    ("seven", "7");
    ("eight", "8");
    ("nine",  "9");
]

let num_words_rev = List.map (fun (a, b) -> (reverse_string a), b) num_words

let get_first_result reslist tlist =
    match unwrap_result reslist with
    | [] -> failwith "no results?"
    | x::_ -> match x with
        | PString s -> let _, digit = List.find (fun (word, _) -> String.equal s word) tlist in digit
        | Char c -> String.make 1 c
        | _ -> failwith "There should be no other parsed values"

let digify_string str =
    let parser = expect_option (List.map (fun (a, _) -> a) num_words) >=>? expect_digit >=>? skip any1 |> at_least_one in
    let rev_parser = expect_option (List.map (fun (a, _) -> a) num_words_rev) >=>? expect_digit >=>? skip any1 |> at_least_one in
    let parsed = run_string_parser parser str in
    let rev_parsed = run_string_parser rev_parser (reverse_string str) in
    get_first_result parsed num_words ^ get_first_result rev_parsed num_words_rev


let part_2 lines =
    lines |> List.to_seq |> Seq.map digify_string |> Seq.map int_of_string |> Seq.fold_left (+) 0
