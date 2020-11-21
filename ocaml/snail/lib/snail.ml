(* 
  Algorithm:
  1. Get dimensions of the map
  2. Get start position
  3. Make circle path
    3.1 Start at current postion and take position value
    3.2 Find next direction
    3.3 Find next position (make turns when appropriate)
    3.4 Repat until current position equals start position 
  4. Reduce width and height by two
  5. Increase start postion indices by one, "dive" by diagonal
  6. Go to 3 and repeat until there are no dimensions
*)
let nth, length, rev = List.(nth, length, rev)

type direction = East | West | North | South

type position = int * int

let move (x, y) direction =
  match direction with
  | East -> (x + 1, y)
  | West -> (x - 1, y)
  | North -> (x, y - 1)
  | South -> (x, y + 1)

let find_direction (start_x, start_y) (x, y) width height direction =
  match
    ( direction
    , x - start_x = width - 1
    , y - start_y = height - 1
    , start_x = x
    , start_y = y )
  with
  | East, true, _, _, _ -> South
  | South, _, true, _, _ -> West
  | West, _, _, true, _ -> North
  | _ -> direction

let step map (x, y) = nth (nth map y) x

let height = length

let width map = nth map 0 |> length

let rec circle map width height start current direction steps =
  let should_stop = current = start && length steps <> 0 in
  if should_stop then steps
  else
    let steps = steps @ [step map current] in
    let next_direction = find_direction start current width height direction in
    let next_position = move current next_direction in
    circle map width height start next_position next_direction steps

let rec path map width height (x, y) steps =
  match (width, height) with
  | _, _ when width <> height -> steps
  | 0, 0 -> steps
  | 1, 1 -> steps @ [step map (x, y)]
  | _ ->
      let circle_steps = circle map width height (x, y) (x, y) East [] in
      let new_width = width - 2 in
      let new_height = height - 2 in
      let new_position = (x + 1, y + 1) in
      let new_steps = steps @ circle_steps in
      path map new_width new_height new_position new_steps

let snail map = path map (width map) (height map) (0, 0) []
