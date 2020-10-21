let init, rev, exists, filter = List.(init, rev, exists, filter)

let split, regexp_string = Str.(split, regexp_string)

let concat = String.(concat)

let range start finish = init (finish - start + 1) (( + ) start)

let divides number divider = number mod divider = 0

let is_prime number =
  match number with
  | 0 | 1 -> false
  | 2 -> true
  | n -> n - 1 |> range 2 |> exists (divides number) |> not

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
