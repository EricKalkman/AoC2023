open Parsing

let card_to_int c =
  (* for ranking cards/hands by cards *)
  match c with
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | n -> int_of_string @@ String.make 1 n

module CM = Map.Make (Char)

(* counts is a map of cards (chars) to the number of those cards *)
type hand = { cards : string; bid : int; counts : int CM.t }

let compare_card cti c1 c2 = compare (cti c1) (cti c2)

type hand_score =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | Yahtzee

(* used for sorting results *)
let int_of_hand_score h =
  match h with
  | HighCard -> 0
  | OnePair -> 1
  | TwoPair -> 2
  | ThreeOfKind -> 3
  | FullHouse -> 4
  | FourOfKind -> 5
  | Yahtzee -> 6

let compare_hand_score a b = compare (int_of_hand_score a) (int_of_hand_score b)

let sort_hand_counts counts =
  (* takes the map of card -> counts and sorts the values of the map (descending) *)
  CM.to_seq counts |> Seq.map snd |> List.of_seq
  |> List.sort (fun n1 n2 -> compare n2 n1)
  |> List.to_seq

let n_of_a_kind n sorted_nums =
  (* checks if there exists a card that has a count of exactly n *)
  (* sorted_nums is a list of numbers of the different kinds of cards, sorted
      in a descending order *)
  match sorted_nums |> Seq.drop_while (fun cnt -> cnt > n) |> Seq.uncons with
  | None -> false
  | Some (cnt, _) -> cnt == n

let tupper sorted_nums =
  (* checks for two pairs *)
  match Seq.take 2 sorted_nums |> List.of_seq with
  | n1 :: n2 :: _ -> n1 == 2 && n2 == 2
  | _ -> false

let score_hand h =
  let sorted = sort_hand_counts h.counts in
  if n_of_a_kind 5 sorted then Yahtzee
  else if n_of_a_kind 4 sorted then FourOfKind
  else if n_of_a_kind 3 sorted && n_of_a_kind 2 sorted then FullHouse
  else if n_of_a_kind 3 sorted then ThreeOfKind
  else if tupper sorted then TwoPair
  else if n_of_a_kind 2 sorted then OnePair
  else HighCard

let compare_hands cmpcard promoter h1 h2 =
  (* cmpcard and promoter complications resulting from Part 2 *)
  (* cmpcard is a comparer function that compares two cards. It is necessary because
     Part 2 ranks Joker differently from Part 1 *)
  (* promoter is a function that takes a hand and its base score and changes
     ("promotes") the score in some way *)
  match
    compare_hand_score
      (score_hand h1 |> promoter h1)
      (score_hand h2 |> promoter h2)
  with
  (* if the two hands have the same score, rank them by the constituent cards *)
  | 0 -> (
      (* discard the first part of each hand that is equal to that of the other *)
      let first_diff =
        Seq.zip (h1.cards |> String.to_seq) (h2.cards |> String.to_seq)
        |> Seq.drop_while (fun (a, b) -> a == b)
      in
      match Seq.uncons first_diff with
      | None -> failwith "Hands are equal"
      (* when the first different card is encountered, use cmpcard to order them *)
      | Some ((a, b), _) -> cmpcard a b)
  | x -> x

let input_parser =
  expect_set "23456789TJQKA" |> repeat 5 |> group >=> skip_whitespace
  >=> expect_int >=> skip_whitespace |> group |> at_least_one

let process_input inp =
  let count_cards str =
    String.to_seq str
    |> Seq.fold_left
         (fun m c ->
           CM.update c
             (fun v -> match v with Some x -> Some (x + 1) | _ -> Some 1)
             m)
         CM.empty
  in
  run_string_parser input_parser inp
  |> unwrap_result
  (* format the input into a list of hand records *)
  |> List.map (fun g ->
         match unwrap_group g with
         | [ Group chrlist; Int bid ] ->
             let cards =
               chrlist |> List.to_seq |> Seq.map unwrap_char |> String.of_seq
             in
             { cards; bid; counts = count_cards cards }
         | _ -> failwith "invalid input")

let part cti promote hands =
  (* helper function for the two parts, which differ by the way they rank cards
     (described by the cti (= card-to-int) function) and whether/how they promote
     card scores using the promote function *)
  hands
  (* sort hands (ascending order) *)
  |> List.sort (compare_hands (compare_card cti) promote)
  |> List.to_seq
  (* multiply each hand's rank by its bid *)
  |> Seq.mapi (fun rank h -> (rank + 1) * h.bid)
  |> Seq.fold_left ( + ) 0

let part_1 inp = process_input inp |> part card_to_int (fun _ x -> x)

(* new card comparison function for Part 2, ranking Jokers low *)
let card_to_int2 c = match c with 'J' -> 1 | x -> card_to_int x

let rec promote_n_jokers score n =
  (* For any hand with Jokers, there are two facts on how to use them as wildcards:
     1. It is always best to promote the Joker is as another copy of the non-Joker card
        with the greatest count.
     2. The greatest count of all non-Joker cards is unambiguous represented by the score
        of the truncated hand not containing the Joker.
     As a result, we do not need any more information than the score without the Joker to
     to know what the score from promoting the Joker will be. We do not need to keep track
     of specific cards. Therefore, by induction, we can start from the score of the hand not
     counting the Jokers and sequentially "add" all Jokers one by one to the hand, updating the
     score as we go according to the best promotion *)
  if n <= 0 then score
  else
    promote_n_jokers
      (match score with
      | HighCard -> OnePair
      | OnePair -> ThreeOfKind
      | TwoPair -> FullHouse
      | ThreeOfKind -> FourOfKind
      | FullHouse -> failwith "unreachable: full house"
      | FourOfKind -> Yahtzee
      | Yahtzee -> Yahtzee)
      (* only occurs when entire hand is jokers *)
      (n - 1)

let promote_result hnd score =
  (* determine the number of Jokers, then promote the score that many times *)
  String.to_seq hnd.cards
  |> Seq.filter (fun c -> c == 'J')
  |> Seq.length |> promote_n_jokers score

let part_2 inp =
  process_input inp
  (* Hack: update the card -> count map removing the Jokers to reuse the scoring
     algorithm from Part 1, but preserve `cards` so that promote_result can still
     figure out how many Jokers there are and tiebreakers can still work *)
  |> List.map (fun h ->
         {
           cards = h.cards;
           bid = h.bid;
           counts = CM.filter (fun c _ -> c != 'J') h.counts;
         })
  |> part card_to_int2 promote_result
