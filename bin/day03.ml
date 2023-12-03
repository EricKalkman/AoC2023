open Aoc2023

let () =
  let lines = Common.read_input_file Sys.argv.(1) in
  Printf.printf "Part 1: %d\n" (Day03.part_1 lines);
  Printf.printf "Part 2: %d\n" (Day03.part_2 lines)
