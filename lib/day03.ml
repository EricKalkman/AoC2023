open Common
open Parsing

let tokenize_engine lines =
    let engine_line_tokenizer =
        expect_char '.' >=>? expect_int >=>? any1
        |> at_least_one
    in
    List.map (fun line ->
                run_string_parser engine_line_tokenizer line |> unwrap_result)
             lines

type engine_datum =
    Empty
    | Number of int
    | Part of char

type coord = int * int
let compare_point (row1, col1) (row2, col2) =
    if row1 < row2 then -1
    else if row1 > row2 then 1
    else if col1 < col2 then -1
    else if col1 > col2 then 1
    else 0

module CoordM = Map.Make(struct type t = coord let compare = compare_point end)
module CoordS = Set.Make(struct type t = coord let compare = compare_point end)

type engine_data = {
    datum : engine_datum;
    row   : int;
    col   : int
}

let process_token_row row_idx row =
    row
    (* normally would conver to Seq here, but Seq doesn't have fold_left_map *)
    (* traverses the row, incrementing the col_idx as it goes *)
    (* data are marshalled into the engine_data structure *)
    |> List.fold_left_map (fun col_idx token ->
            match token with
                (* skip '.' *)
                | Char '.' -> col_idx+1, { datum = Empty; row = row_idx; col = col_idx }
                (* some kind of part *)
                | Char c   -> col_idx + 1, { datum = Part c; row = row_idx; col = col_idx }
                (* number; increment col_idx by the length of the string *)
                | Int n    -> col_idx + String.length (string_of_int n),
                              { datum = Number n; row = row_idx; col = col_idx }
                | _ -> failwith "Unrecognized token")
        0
    |> snd

let process_engine engine_lines =
    engine_lines
    |> List.fold_left_map (fun row_idx row -> row_idx+1, process_token_row row_idx row |> List.to_seq) 0
    (* drop the final value of the accumulator from fold_left_map *)
    |> snd
    (* combine all lines into a single sequence of engine_data *)
    |> List.to_seq |> Seq.concat
    (* remove '.' *)
    |> Seq.filter (fun x -> match x.datum with | Empty -> false | _ -> true)
    (* split off parts and numbers into their own sequences *)
    |> Seq.partition (fun x -> match x.datum with | Part _ -> true | _ -> false)

let span_of_int num =
    (* returns a sequence of coord's representing the coords that num covers *)
    match num.datum with
    | Number n -> make_range num.col (num.col + (String.length (string_of_int n) - 1))
                    |> Seq.map (fun col -> (num.row, col), n)
    | _ -> failwith "unreachable"

let add_num_to_point_cloud num = CoordM.add_seq @@ span_of_int num
(* returns a mapping of coord -> num *)
let create_point_cloud_of_nums = Seq.fold_left (fun m n -> add_num_to_point_cloud n m) CoordM.empty
(* returns a set of coord's covered by parts *)
let create_point_cloud_of_parts parts =
    let coords = parts |> Seq.map (fun g -> g.row, g.col) in
    CoordS.of_seq coords

let neighbors (row, col) =
    [ (row-1, col-1); (row-1, col); (row-1, col+1);
      (row, col-1);                 (row, col+1);
      (row+1, col-1); (row+1, col); (row+1, col+1) ] |> List.to_seq

let num_border num =
    (* returns coords of all tiles surrounding num *)
    let num_span = span_of_int num |> Seq.map fst |> CoordS.of_seq in
    (* hackish way to get the border: get neighbors of all points of integer span, then
     * shove them into a set to deduplicate, and subtract off any points that are contained
     * in the span *)
    let all_points = CoordS.to_seq num_span |> Seq.flat_map neighbors |> CoordS.of_seq in
    CoordS.diff all_points num_span

let associated_part partset num =
    (* determines the coord at which a part is near num *)
    num_border num
    (* remove all border coords that don't correspond to a part by searching the part set *)
    |> CoordS.to_seq |> Seq.filter_map (fun c -> CoordS.find_opt c partset) |> List.of_seq

let unwrap_num num =
    match num.datum with
    | Number n -> n
    | _ -> failwith "unreachable"

let part_1 lines =
    let parts, nums = tokenize_engine lines |> process_engine in
    let partset = create_point_cloud_of_parts parts in
    nums
    (* create pairs of (num, coord of associated part) *)
    |> Seq.map (fun n -> unwrap_num n, associated_part partset n)
    (* remove numbers with no associated parts *)
    |> Seq.filter (fun (_, coord_lst) -> List.length coord_lst > 0)
    (* take only the numbers *)
    |> Seq.map fst
    |> Seq.fold_left (+) 0

let associated_numbers nummap part =
    neighbors (part.row, part.col)
    |> Seq.filter_map (fun c -> CoordM.find_opt c nummap) |> List.of_seq |> List.sort_uniq compare

let part_2 lines =
    let parts, nums = tokenize_engine lines |> process_engine in
    (* map taking coord -> number at that coord *)
    let nummap = create_point_cloud_of_nums nums in
    (* filter for parts that have a '*' symbol *)
    let gears = Seq.filter (fun p -> match p.datum with | Part '*' -> true | _ -> false) parts in
    gears
    |> Seq.map (associated_numbers nummap)             (* get gears' associated numbers *)
    |> Seq.filter (fun nums -> List.length nums == 2)  (* filter out *'s that don't have two numbers for ratio *)
    |> Seq.map (fun nums -> List.fold_left ( * ) 1 nums)
    |> Seq.fold_left (fun acc num -> acc + num) 0
