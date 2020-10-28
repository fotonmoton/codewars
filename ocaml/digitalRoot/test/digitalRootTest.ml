open Alcotest
open DigitalRoot

let check_int = check int

let cases = [(0, 0); (16, 7); (195, 6); (992, 2); (999999999, 9); (167346, 9)]

let check (num, expected) =
  let f () = check_int "" expected (digital_root num) in
  test_case (string_of_int num) `Quick f

let suite = [("cases", cases |> List.map check)]

let () = run "Digital Root" suite
