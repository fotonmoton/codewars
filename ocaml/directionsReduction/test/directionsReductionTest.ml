open Alcotest
open DirectionsReduction.Reduction

let string_of_list dirs = dirs |> String.concat "|" |> Format.sprintf "[%s]"

let pp ppf dirs = Fmt.pf ppf "%s" (string_of_list dirs)

let check = check (testable pp ( = )) ""

let cases =
  [ ([], [])
  ; (["WEST"], ["WEST"])
  ; (["WEST"; "EAST"], [])
  ; (["WEST"; "EAST"; "EAST"], ["EAST"])
  ; (["WEST"; "EAST"; "EAST"; "WEST"], [])
  ; (["WEST"; "NORTH"; "SOUTH"; "EAST"], [])
  ; (["NORTH"; "SOUTH"; "SOUTH"; "EAST"; "WEST"; "NORTH"; "WEST"], ["WEST"])
  ; (["NORTH"; "SOUTH"; "SOUTH"; "EAST"; "WEST"; "NORTH"; "NORTH"], ["NORTH"])
  ]

let case (directions, expected) =
  let f () = check expected (reduceDir directions) in
  test_case (string_of_list directions) `Quick f

let suite = [("cases", List.map case cases)]

let () = run "Directions Reduction" suite
