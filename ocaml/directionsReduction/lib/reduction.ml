let rec converge f x = if f x = x then x else converge f (f x)

let opposite a b =
  match (a, b) with
  | "WEST", "EAST" | "EAST", "WEST" -> true
  | "SOUTH", "NORTH" | "NORTH", "SOUTH" -> true
  | _ -> false

let rec pass directions =
  match directions with
  | [] -> []
  | [one] -> [one]
  | one :: two :: rest when opposite one two -> pass rest
  | one :: rest -> one :: pass rest

let reduceDir = converge pass
