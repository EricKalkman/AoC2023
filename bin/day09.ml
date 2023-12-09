open Aoc2023

let () =
  let inp = Common.read_input_file Sys.argv.(1) in
  let p1, p2 = Day09.parts_1_and_2 inp in
  Printf.printf "Part 1: %s\n" (Num.string_of_num p1);
  Printf.printf "Part 2: %s\n" (Num.string_of_num p2)
