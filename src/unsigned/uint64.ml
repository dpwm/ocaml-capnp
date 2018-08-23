open Int64

type t = int64

let add = add

let sub = sub

let mul = mul

let div a b = 
  match ((Int64.compare a 0L) >= 0, (Int64.compare b 0L) >= 0) with
  | (true, true) -> Int64.div a b
  | (true, false) -> 0L
  | (false, true) -> Int64.shift_right_logical a 1 |> (
    fun x -> Int64.div x b) |> (fun x -> Int64.shift_left x 1) |> fun x -> 
      if Int64.compare a (Int64.add (Int64.mul x b) b) >= 0 then Int64.add x 1L else x
  | (false, false) -> if Int64.compare a b >= 0 then 1L else 0L

let compare a b = 
  match ((Int64.compare a 0L) >= 0, (Int64.compare b 0L) >= 0) with
  | (true, true)
  | (false, false) -> compare a b
  | (false, true) -> 1
  | (true, false) -> -1

let to_string x =
  if Int64.compare x 0L > 0 then 
    Int64.to_string x
  else
    let z = div x 100L in
    (z |> Int64.to_string) ^ (Int64.sub x (Int64.mul z 100L) |> Int64.to_string)

let of_string x =
  let l = String.length x in 
  if l <= 19 then 
    Int64.of_string x
  else
    let a = String.sub x 0 (l-2) in
    let b = String.sub x (l-2) 2 in
    Int64.add (Int64.mul (Int64.of_string a) 100L) (Int64.of_string b)

let to_int64 x = x
let of_int64 x = x
