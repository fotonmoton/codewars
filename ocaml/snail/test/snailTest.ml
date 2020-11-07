open Alcotest
open Snail

let cases =
  [ ([[1; 2; 3]; [4; 5; 6]; [7; 8; 9]], [1; 2; 3; 6; 9; 8; 7; 4; 5])
  ; ([[1; 2; 3]; [8; 9; 4]; [7; 6; 5]], [1; 2; 3; 4; 5; 6; 7; 8; 9]) ]

let paths =
  let check (input, expected) =
    let f () = check (list int) "" expected (snail input) in
    test_case "" `Quick f
  in
  ("cases", List.map check cases)

let correct_length () =
  check int "" 9 (length [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]])

let correct_step () =
  check int "" 5 (step [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] (1, 1))

let suite =
  [ ("length", [test_case "correct" `Quick correct_length])
  ; ("step", [test_case "correct" `Quick correct_step]) ]

let () = run "Snail" suite
