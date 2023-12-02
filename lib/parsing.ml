open Common

(* Expected * found *)
type parse_error =
    | EndOfString
    | WrongChar of char * char
    | WrongString of string * string
    | WrongCharSet of string * char
    | WrongStringSet of string list * string
    | NotInt of char
    | FailedPred of char
    | EmptyStack
    | IntentionalFail

let err_str err =
    match err with
    | EndOfString -> "EndOfString"
    | WrongChar (ex, fnd) -> Printf.sprintf "Expected char %s; found %s" (char_to_str ex) (char_to_str fnd)
    | WrongString (ex, fnd) -> Printf.sprintf "Expected string %s; found %s" ex fnd
    | WrongCharSet (set, fnd) -> Printf.sprintf "Expected char in %s; found %s" set (char_to_str fnd)
    | WrongStringSet (set, fnd) -> Printf.sprintf "Expected string in %s; found %s"
                                        (String.concat ", " set) fnd
    | NotInt c -> Printf.sprintf "Expected an int, got %s" (char_to_str c)
    | FailedPred c -> Printf.sprintf "Predicate match failed on char %s" (char_to_str c)
    | EmptyStack -> "EmptyStack"
    | IntentionalFail -> "IntentionalFail"

type parsed_item =
    | Sentinel
    | Int of int
    | Char of char
    | PString of string
    | Group of parsed_item list

type parse_stack = parsed_item list

type parse_state =
    | Failure of parse_error
    | Success of parse_stack * char Seq.t

let return str = Success ([], String.to_seq str)

(* Monadic bind for parse state ("and then" operation) *)
let (>>=) res p =
    match res with
    | Failure e -> Failure e
    | Success (stack, xs) -> p stack xs

(* Kleisli composition for parse state
   at least, I think that's what it's called *)
let (>=>) p1 p2 stack sq =
    Success (stack, sq) >>= p1 >>= p2

let (>=>?) p1 p2 stack sq =
    match p1 stack sq with
    | Failure _ -> p2 stack sq
    | succ -> succ

let map_error fn p stack sq =
    match p stack sq with
    | Failure e -> Failure (fn e stack sq)
    | succ -> succ

let push_stack x stack sq =
    Success (List.cons x stack, sq)

let pop_stack stack sq =
    match stack with
    | [] -> Failure EmptyStack
    | _::xs -> Success (xs, sq)

let mod_top fn stack sq =
    match stack with
    | [] -> Failure EmptyStack
    | x::xs -> Success (List.cons (fn x) xs, sq)

let mod_stack fn stack sq =
    Success (fn stack, sq)


let rec some p stack sq =
    match p stack sq with
    | Success (ns, nq) -> some p ns nq
    | _ -> Success (stack, sq)

let at_least_one p =
    p >=> some p

let nop state sq =
    Success (state, sq)

let fail _ _ =
    Failure IntentionalFail

let maybe p =
    p >=>? nop

let any1 state sq =
    match Seq.uncons sq with
    | None -> Failure EndOfString
    | Some(c, ns) -> Success (List.cons (Char c) state, ns)

let skip p stack sq =
    p stack sq >>= mod_stack (fun _ -> stack)

let group p stack sq =
    match Success (stack, sq) >>= push_stack Sentinel >>= p with
    | Success (ns, nq) ->
            let ns_seq = List.to_seq ns in
            let group = ns_seq |> Seq.take_while (fun x -> x != Sentinel) |> List.of_seq |> List.rev in
            let rest = ns_seq |> Seq.drop_while (fun x -> x != Sentinel) |> List.of_seq in
            Success (List.cons (Group group) (List.tl rest), nq)
    | e -> e

let repeat n p =
    Seq.repeat p
    |> Seq.take n
    |> Seq.fold_left (>=>) nop

let expect_char c stack sq =
    match Seq.uncons sq with
    | None -> Failure EndOfString
    | Some (h, xs) ->
            if h == c then Success (List.cons (Char c) stack, xs)
            else Failure (WrongChar (c, h))

let expect_pred pred stack sq =
    match Seq.uncons sq with
    | None -> Failure EndOfString
    | Some (h, xs) ->
            if pred h then Success (List.cons (Char h) stack, xs)
            else Failure (FailedPred h)

let expect_set str =
    expect_pred (fun c -> String.contains str c)
    |> map_error (fun e _ _ -> match e with
        | FailedPred c -> WrongCharSet (str, c)
        | e -> e)

let expect_digit = expect_set "0123456789"
let expect_lower = expect_set "abcdefghijklmnopqrstuvwxyz"
let expect_upper = expect_set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let expect_letter = expect_set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let stringify_group g =
    match g with
    | Group lst -> List.to_seq lst
        |> Seq.map (fun cr -> match cr with
                    | Char c -> c
                    | _ -> failwith "unimplemented")
        |> String.of_seq
    | _ -> failwith "unimplemented"

let unwrap_group g =
    match g with
    | Group lst -> lst
    | _ -> failwith "Attempted to unwrap a group"
let group_to_pair g =
    match unwrap_group g with
    | [] | _::[] -> failwith "Failed to make a pair out of a group"
    | a::b::_ -> a, b

let expect_string str =
    String.to_seq str
    |> Seq.map expect_char
    |> Seq.fold_left (>=>) nop
    |> map_error (fun _ _ sq ->
            WrongString (str,
                         sq |> Seq.take (String.length str) |> String.of_seq))
    |> group
    >=> mod_top (fun g -> PString (stringify_group g))

let skip_string str = expect_string str |> skip

let expect_option strlist =
    List.to_seq strlist |> Seq.map expect_string |> Seq.fold_left (>=>?) fail
    |> map_error (fun e _ _ -> match e with
        | WrongString (_, fnd) -> WrongStringSet (strlist, fnd)
        | e -> e)

let expect_int =
    maybe (expect_char '-') >=> at_least_one expect_digit
    |> group
    |> map_error (fun e _ _ -> match e with
        | WrongChar (_, fnd) -> NotInt(fnd)
        | WrongCharSet (_, fnd) -> NotInt(fnd)
        | e -> e)
    >=> mod_top (fun g -> Int (g |> stringify_group |> int_of_string))

(* does not automatically group items! *)
let expect_list data_p delim_p =
    (data_p >=> skip delim_p |> some)
    >=> maybe data_p

let skip_whitespace =
    expect_set "\n\r\t " |> skip

let stringify_top stack sq =
    match stack with
    | [] -> Failure EmptyStack
    | top :: xs -> Success(List.cons (match top with
        | Sentinel  -> PString "<sentinel>"
        | Int n     -> PString (string_of_int n)
        | Char c    -> PString (String.make 1 c)
        | PString s -> PString s
        | Group lst -> PString (stringify_group (Group lst))) xs,
        sq)

let run_parser p sq =
    match p [] sq with
    | Success (stack, sq) -> Success (List.rev stack, sq)
    | e -> e

let run_string_parser p text =
    match return text >>= p with
    | Success (stack, sq) -> Success (List.rev stack, sq)
    | e -> e

let unwrap_result res =
    match res with
    | Failure _ -> failwith "Attempted to unwrap a failed parse result"
    | Success (lst, _) -> lst

let unwrap_int r =
    match r with
    | Int n -> n
    | _ -> failwith "Tried to unwrap an int"

let unwrap_group g =
    match g with
    | Group lst -> lst
    | _ -> failwith "Attempted to unwrap a group"

let unwrap_ps ps =
    match ps with
    | PString s -> s
    | _ -> failwith "encountered parse result that was not a pstring"

