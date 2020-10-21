let () =
  BackwardsPrime.backwards_prime 0 15000
  |> List.map string_of_int
  |> String.concat ", "
  |> Printf.printf "%s"
