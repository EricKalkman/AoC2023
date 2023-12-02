open Alcotest
open Aoc2023

let test_part fn file expected =
    let lines = Common.read_other_file ("../../../inputs/" ^ file) in
    let result = fn lines in
    check int "same int" result expected


let day01_part1 () = test_part Day01.part_1 "day01.test" 142

let day01_part2 () = test_part Day01.part_2 "day01-2.test" 281

let day02_part1 () = test_part Day02.part_1 "day02.test" 8

let day02_part2 () = test_part Day02.part_2 "day02.test" 2286

let () =
    print_endline (Sys.getcwd ());
    run "Testing All Days" [
        "Day 1", [
            "part 1", `Quick, day01_part1;
            "part 2", `Quick, day01_part2
        ];
        "Day 2", [
            "part 1", `Quick, day02_part1;
            "part 2", `Quick, day02_part2
        ]
    ]

