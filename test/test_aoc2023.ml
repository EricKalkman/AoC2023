open Alcotest

let () =
  print_endline (Sys.getcwd ());
  run "Testing All Days" []
