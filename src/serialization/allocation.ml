(* Nearly all our complexity comes from our half-baked multi-copy approach to
 * allocation.*)

(* We can make our interface approximately zero-copy by merging the properties
 * of a tree with the properties of a byte array. It will be posisble to use
 * this in a zero-copy way, using iter, and producing an iovector. For windows
 * support, we will have to resort to collapsing. *)

type t =
  | Buffer of Data.t
  | Join of int * int * t * t

let length = function
  | Buffer data -> Data.length data
  | Join (a, b, _, _) -> a + b

let rec iter f = function
  | Buffer data -> f data
  | Join (_, _, x, y) ->
      iter f x;
      iter f y

let of_data x = Buffer x
let to_data x =
  let buf = Data.Buffer.make (length x) in
  iter (Data.Buffer.add buf) x;
  Data.Buffer.contents buf

let flatten x = x |> to_data |> of_data

let of_string x = x |> Data.of_string |> of_data

let to_string x = x |> to_data |> Data.to_string

let cat a b = Join (length a, length b, a, b)


let rec find x n =
  match x with
  | Buffer x ->
    (x, n)
  | Join (a, b, x, y) ->
    let v = n - a in
    if v >= 0 then
      if v >= b then (raise (Invalid_argument "n is out of bounds"))
      else find y v
    else find x n

let get x n =
  let (xs, i) = find x n in
  Data.get xs i

let () =
  let v = cat
    (cat (of_string "Hello") (of_string " "))
    (of_string "world!") in
  let v = cat (of_string "Message: ") v in
  let out = to_string v in
  Printf.printf "%S\n" out;
  for i = 0 to length v - 1 do
    get v i |> Printf.printf "%c"
  done
