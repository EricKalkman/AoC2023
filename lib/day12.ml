open Parsing
open Common

let line_parser =
  expect_set ".#?" |> at_least_one |> group >=> stringify_top
  >=> skip_whitespace
  >=> (expect_list expect_int (expect_char ',') |> group)
  >=> skip_whitespace

type spring = N | D | Q (* normal, damaged, questionable *)

let process_input inp =
  List.map
    (fun ln ->
      match run_string_parser line_parser ln |> unwrap_result with
      | [ PString str; Group intlist ] ->
          ( String.to_seq str
            |> Seq.map (fun c ->
                   match c with
                   | '.' -> N
                   | '#' -> D
                   | '?' -> Q
                   | _ -> failwith "bad char")
            |> List.of_seq,
            List.map unwrap_int intlist )
      | _ -> failwith "Could not parse input")
    inp

let split_at n lst =
  let rec split_at' acc n lst =
    if n <= 0 then Some (List.rev acc, lst)
    else
      match lst with
      | [] -> None
      | _ -> split_at' (List.cons (List.hd lst) acc) (n - 1) (List.tl lst)
  in
  split_at' [] n lst

let solve springs rngs =
  (* I'm sure there's an optimal value here. I don't really care to optimize *)
  let ht = Hashtbl.create 1024 in
  let rec solve' springs rngs =
    let res =
      (* check for memoized result *)
      match Hashtbl.find_opt ht (springs, rngs) with
      | Some x -> x
      | None -> (
        (* if no memoized result *)
          match (springs, rngs) with
          | [], [] -> 1
          | [], _ -> 0
          (* damaged springs remaining, but no ranges remaining; invalid state *)
          | D :: _, [] -> 0
          (* normal spring encountered; ranges are not affected *)
          | N :: ss, _ -> solve' ss rngs
          (* damaged spring encountered... *)
          | D :: _, r :: rs -> (
              (* try to split off the first r elements of the list of springs *)
              match split_at r springs with
              (* failure means we cannot match with the range of damaged springs defined by r *)
              | None -> 0
              | Some (prefix, next_nondamaged) ->
                  (* if the next r springs are either damaged or unknown, then they satisfy the
                     range criterion *)
                  if all (fun s -> List.mem s [ D; Q ]) (List.to_seq prefix)
                  then
                    match next_nondamaged with
                    (* the next spring cannot be damaged; force it to be normal if ? *)
                    | D :: _ -> 0
                    | Q :: ss -> solve' (N :: ss) rs
                    | _ -> solve' next_nondamaged rs
                  else 0)
          (* ? spring encountered; count solutions for both it being a damaged spring and a
             normal spring *)
          | Q :: ss, _ -> solve' (D :: ss) rngs + solve' (N :: ss) rngs)
    in
    (* add the result to the has table *)
    Hashtbl.add ht (springs, rngs) res;
    (* return the result *)
    res
  in
  solve' springs rngs

let part_1 inp =
  let springs, rngs = process_input inp |> List.split in
  List.map2 solve springs rngs |> List.fold_left ( + ) 0

let part_2 inp =
  let n_repeats = 5 in
  let springs, rngs = process_input inp |> List.split in
  (* intersperse copies of lists of springs with "?" *)
  let springs =
    List.map
      (fun ss ->
        Seq.zip (Seq.repeat Q) (Seq.repeat (List.to_seq ss))
        |> Seq.take n_repeats
        |> Seq.flat_map (fun (g, lst) -> Seq.cons g lst)
        |> Seq.drop 1 |> List.of_seq)
      springs
  in
  let rngs =
    List.map
      (fun r ->
        List.to_seq r |> Seq.repeat |> Seq.take n_repeats |> Seq.concat
        |> List.of_seq)
      rngs
  in
  List.map2 solve springs rngs |> List.fold_left ( + ) 0
