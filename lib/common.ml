
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

let first_of_pair (a,_) = a
let snd_of_pair (_,b) = b
