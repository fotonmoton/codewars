let rev, fold_left, filter = List.(rev, fold_left, filter)

let split, regexp_string = Str.(split, regexp_string)

let range = Range.(range)

let is_prime = Prime.(is_prime_aks)

let parmap = Parmap.(parmap)

let concat = String.(concat)

let flip number =
  number
  |> string_of_int
  |> split (regexp_string "")
  |> rev
  |> concat ""
  |> int_of_string

let is_palindrome num = num = flip num

let backwards num =
  (not (is_palindrome num)) && is_prime num && is_prime (flip num)

let backwards_prime start finish = range start finish |> filter backwards

let backwards_prime_parallel start finish =
  let marked =
    parmap ~ncores:32
      (fun num -> (num, backwards num))
      (Parmap.L (range start finish))
  in
  marked
  |> fold_left (fun acc (num, prime) -> if prime then num :: acc else acc) []
  |> rev
