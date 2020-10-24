open Alcotest

let cases =
  [ (7000, 7100, [7027; 7043; 7057])
    (* ; (70000, 70245, [70001; 70009; 70061; 70079; 70121; 70141; 70163; 70241]) *)
    (* ; (7000, 7100, [7027; 7043; 7057]) *)
    (* (70000, 70245, [70001; 70009; 70061; 70079; 70121; 70141; 70163; 70241])  *)
  ]

let check (start, finish, expected) =
  let check_list = check (list int) in
  let f () =
    check_list "" expected (BackwardsPrime.backwards_prime start finish)
  in
  test_case (string_of_int start ^ ":" ^ string_of_int finish) `Quick f

let tests = ("backwards_prime", List.map check cases)

let () = run "BackwardsPrime" [tests]
