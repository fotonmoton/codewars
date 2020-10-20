let (init, fold_left, map) = List.(init, fold_left, map)

let id a = a

let generate n = init n id

let repeat num count = init count (fun _ -> num)

(* only for powers > 0 :) *)
let exp num power = fold_left ( * ) 1 (repeat num power)

let powersOfTwo n = (n + 1) |> generate |> map (exp 2)