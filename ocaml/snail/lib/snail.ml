(* 
  Algorithm:
  1. Get dimensions of the map
  2. Get position and end positions for straight line
  3. Iterate from position to end position collecting path values untile position <> end
  4. Find next direction
  5. update dimensions
  6. go to 2. until both dimensions <> 0 
*)
type direction = East | West | North | South

type position = int * int

type line = position * position

let move (x, y) direction =
  match direction with
  | East -> (x + 1, y)
  | West -> (x - 1, y)
  | North -> (x, y - 1)
  | South -> (x, y + 1)

let next_direction direction =
  match direction with
  | East -> South
  | South -> West
  | West -> North
  | North -> East

let find_line direction ((x1, y1), (x2, y2)) width height =
  match direction with
  | East -> ((x1, y1), (x2 + width, y2))
  | South -> ((x2, y2), (x2, y2 + height))
  | West -> ((x2, y2), (x2 - width, y2))
  | North -> ((x2, y2), (x2, y2 - height))

let update_dimensions width height direction =
  match direction with
  | East | West -> (width, height - 1)
  | South | North -> (width - 1, height)

let should_stop ((x1, y1), (x2, y2)) = x1 = x2 || y1 = y2

let no_dimensions width height = width = 0 || height = 0

let step map (x, y) = List.nth (List.nth map y) x

let height = List.length

let width map = List.nth map 0 |> List.length

let rec collect_line (position, finish) direction map steps =
  if should_stop (position, finish) then step map position :: steps
  else
    let new_position = move position direction in
    let step = step map position in
    collect_line (new_position, finish) direction map (step :: steps)

let rec path width height direction line map steps =
  if no_dimensions width height then steps
  else
    let new_steps = collect_line line direction map steps in
    let new_width, new_height = update_dimensions width height direction in
    let next_direction = next_direction direction in
    let new_line = find_line next_direction line new_width new_height in
    path new_width new_height next_direction new_line map (new_steps @ steps)

let snail map =
  let width = width map in
  let height = height map in
  let start_line = find_line East ((0, 0), (0, 0)) width height in
  path width height East start_line map []
