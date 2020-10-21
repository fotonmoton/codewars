module Tests = struct
  open BackwardsPrime
  open OUnit

  let print_list lst =
    lst |> List.map string_of_int |> String.concat ", " |> Printf.printf "%s"

  let testing (m : int) (n : int) (expectedOutput : int list) =
    let act = backwards_prime m n in
    print_string "input " ;
    print_int m ;
    print_string "\n" ;
    print_int n ;
    print_string "\n" ;
    print_string "Expected " ;
    print_list expectedOutput ;
    print_endline "\n got " ;
    print_list act ;
    print_endline "\n-----" ;
    print_endline "" ;
    assert_equal expectedOutput act

  let suite =
    "backwards_prime"
    >::: [ ( "Basic tests"
           >:: fun _ ->
           testing 7000 7100 [7027; 7043; 7057] ;
           testing 70000 70245
             [70001; 70009; 70061; 70079; 70121; 70141; 70163; 70241] ;
           testing 7000 7100 [7027; 7043; 7057] ;
           testing 70000 70245
             [70001; 70009; 70061; 70079; 70121; 70141; 70163; 70241] ) ]
end

let _ = OUnit.run_test_tt_main Tests.suite
