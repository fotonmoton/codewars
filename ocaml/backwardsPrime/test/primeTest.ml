open Alcotest
open Prime

let ( >> ) f g x = g (f x)

let pascal_row_cases =
  [(4, [|1; 3; 3; 1|]); (10, [|1; 9; 36; 84; 126; 126; 84; 36; 9; 1|])]

let test_pascal_row variant (nth, row) =
  let check_array = check (array int) in
  let name = string_of_int nth ^ " row" in
  let f () = check_array "" row (variant nth) in
  test_case name `Quick f

let pascal_row_fast =
  let to_int_list = Array.map Big_int.int_of_big_int in
  let cases =
    pascal_row_cases |> List.map (test_pascal_row (pascal_row >> to_int_list))
  in
  ("pascal_row_fast", cases)

let pascal_row_slow =
  let cases = pascal_row_cases |> List.map (test_pascal_row pascal_row_slow) in
  ("pascal_row_slow", cases)

let test_is_prime num =
  let check_bool = check bool in
  let name = string_of_int num ^ " is prime" in
  let f () = check_bool "" true (is_prime_aks num) in
  test_case name `Quick f

let is_prime_aks =
  let cases = [2; 3; 5; 7; 97; 7027] |> List.map test_is_prime in
  ("is_prime_aks", cases)

let () = run "Prime" [is_prime_aks; pascal_row_fast; pascal_row_slow]
