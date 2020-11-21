(* 
  Algorithm:
  1. Get dimensions of the map
  2. Get start and end positions for straight line
  3. Iterate from position to end position collecting path values until position <> end
  4. Find next direction
  5. update dimensions
  6. go to 2. until both dimensions <> 0 
*)
let nth, length, rev = List.(nth, length, rev)

type direction = East | West | North | South

type position = int * int

let direction_to_string dir =
  match dir with
  | East -> "East"
  | West -> "West"
  | South -> "South"
  | North -> "North"

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

let rec circle_path map width height start_position current_position direction
    steps =
  Printf.printf "w:%d, h:%d, x:%d, y:%d -> %s\n" width height
    (fst current_position) (snd current_position)
    (direction_to_string direction) ;
  if current_position = start_position && length steps <> 0 then steps
  else
    let steps = steps @ [step map current_position] in
    let next_direction =
      find_direction start_position current_position width height direction
    in
    let next_position = move current_position next_direction in
    circle_path map width height start_position next_position next_direction
      steps

let rec path map width height (x, y) steps =
  match (width, height) with
  | 0, 0 -> steps
  | 1, 1 -> steps @ [step map (x, y)]
  | _ ->
      let circle_steps = circle_path map width height (x, y) (x, y) East [] in
      path map (width - 2) (height - 2) (x + 1, y + 1) (steps @ circle_steps)

let snail map = path map (width map) (height map) (0, 0) []
