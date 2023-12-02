
let read_input_file fname =
    let fname = "inputs/" ^ fname in
    let lines = ref [] in
    let chan = open_in fname in
    try
        while true; do
            lines := input_line chan :: !lines
        done;
        !lines
    with End_of_file ->
        close_in chan;
        List.rev !lines

let char_to_str c = String.make 1 c

let rec any p lst =
    match lst with
    | [] -> false
    | x :: xs -> if p x then true else any p xs

let rec all p lst =
    match lst with
    | [] -> true
    | x :: xs -> if p x then all p xs else false
