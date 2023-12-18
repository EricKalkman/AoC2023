open Aoc2023

let () =
  let inp = Common.read_input_file Sys.argv.(1) in
  Printf.printf "Part 1: %d\n" (Day17_2.part_1 inp);
  Printf.printf "Part 2: %d\n" (Day17_2.part_2 inp)
