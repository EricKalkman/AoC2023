open Num (* for arbitrary precision, specifically for rationals *)
open Common
open Parsing

let line_parser = expect_list expect_int expect_whitespace

let process_input inp =
  List.to_seq inp
  |> Seq.map @@ run_string_parser line_parser
  |> Seq.map (fun line ->
         line |> unwrap_result |> List.to_seq
         |> Seq.map (fun x -> Num.Int (unwrap_int x))
         |> Array.of_seq)

let lagrange_poly xs ys at_x =
  (* Returns a function that evaluates Lagrange polynomial defined by abscissae xs
     and corresponding ordinates ys. Assumes all xs are distinct *)
  let lagrange_lj j at_x =
    let xj = Array.get xs j in
    Array.to_seq xs
    |> Seq.fold_lefti
         (fun res m xm ->
           if m == j then res else res */ (at_x -/ xm) // (xj -/ xm))
         (Num.Int 1)
  in
  let ljs = make_range 0 (Array.length xs - 1) |> Seq.map lagrange_lj in
  Array.to_seq ys
  |> Seq.fold_left2 (fun res lj yj -> res +/ (yj */ lj at_x)) (Num.Int 0) ljs

let make_lagrange_poly ys =
  (* returns a lagrange polynomial fn calculated from the input ys (the "history") *)
  (* the x coordinates are the 1-based indices of ys *)
  lagrange_poly
    (make_range 1 (Array.length ys) |> Seq.map num_of_int |> Array.of_seq)
    ys

let parts_1_and_2 inp =
  let lens_and_polys =
    process_input inp
    |> Seq.map (fun ys -> (Array.length ys, make_lagrange_poly ys))
    |> List.of_seq
  in
  ( List.fold_left
      (* evaluate the polynomials at 1 + the last index *)
        (fun res (len, p) -> res +/ p (Num.Int (len + 1)))
      (Num.Int 0) lens_and_polys,
    List.fold_left
      (* evaluate the polynomial at 0 *)
        (fun res (_, p) -> res +/ p (Num.Int 0))
      (Num.Int 0) lens_and_polys )
