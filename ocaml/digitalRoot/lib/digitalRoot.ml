let split = Str.split (Str.regexp_string "")

let to_int_list = List.map int_of_string

let sum = List.fold_left ( + ) 0

let rec digital_root number =
  if number < 10 then number
  else number |> string_of_int |> split |> to_int_list |> sum |> digital_root
