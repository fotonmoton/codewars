let () =
  BackwardsPrime.backwards_prime 70000 70245
  |> List.map string_of_int
  |> String.concat ", "
  |> Printf.printf "%s"
