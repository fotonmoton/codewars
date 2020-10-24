let ( >> ) f g x = g (f x)

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t) ;
  flush stdout ;
  fx

let is_prime number =
  let range = Range.(range) in
  let divides number divider = number mod divider = 0 in
  match number with
  | 0 | 1 -> false
  | 2 -> true
  | n -> n - 1 |> range 2 |> List.exists (divides number) |> not

let pascal_row_slow row =
  let make, map2, fold_left, map, append =
    Array.(make, map2, fold_left, map, append)
  in
  let pair a b = (a, b) in
  let construct acc _ =
    let left = append [|0|] acc in
    let right = append acc [|0|] in
    let subtact (a, b) = a - b in
    map2 pair left right |> map subtact
  in
  match row with
  | 1 -> [|1|]
  | n -> make (n - 1) 0 |> fold_left construct [|1|] |> map abs

let pascal_row row_index =
  let make, iteri = Array.(make, iteri) in
  let of_int = Big_int.big_int_of_int in
  let compute prev row index =
    let ( - ) = Big_int.sub_big_int in
    let ( * ) = Big_int.mult_big_int in
    let ( / ) = Big_int.div_big_int in
    let index = of_int index in
    prev * (row - index) / index
  in
  let row = make row_index (of_int 1) in
  let row_index = of_int row_index in
  let set_elem col_index _ =
    match col_index with
    | 0 -> ()
    | i -> row.(i) <- compute row.(i - 1) row_index i
  in
  iteri (time set_elem) row ;
  row

let is_prime_aks number =
  (*
    we can calculate half pascal row
    instead of chopping it afterwars
  *)
  let half arr = Array.sub arr 1 (Array.length arr / 2) in
  let divides number divider =
    Big_int.compare_big_int
      (Big_int.mod_big_int divider number)
      Big_int.zero_big_int
    = 0
  in
  match number with
  | 0 | 1 -> false
  | p ->
      pascal_row (p + 1)
      |> half
      |> Array.for_all (divides (Big_int.big_int_of_int p))
