type t = int32

let add, sub, mul  = Int32.(add, sub, mul)

let div a b = 
  match ((Int32.compare a 0l) >= 0, (Int32.compare b 0l) >= 0) with
  | (true, true) -> Int32.div a b
  | (true, false) -> 0l
  | (false, true) -> Int32.shift_right_logical a 1 |> (
    fun x -> Int32.div x b) |> (fun x -> Int32.shift_left x 1) |> fun x -> 
      if Int32.compare a (Int32.add (Int32.mul x b) b) >= 0 then Int32.add x 1l else x
  | (false, false) -> if Int32.compare a b >= 0 then 1l else 0l

let compare a b = 
  match ((Int32.compare a 0l) >= 0, (Int32.compare b 0l) >= 0) with
  | (true, true)
  | (false, false) -> compare a b
  | (false, true) -> 1
  | (true, false) -> -1

let to_string x =
  if Int32.compare x 0l > 0 then 
    Int32.to_string x
  else
    let z = div x 100l in
    (z |> Int32.to_string) ^ (Int32.sub x (Int32.mul z 100l) |> Int32.to_string)

let of_string x =
  let l = String.length x in 
  if l <= 9 then 
    Int32.of_string x
  else
    let a = String.sub x 0 (l-2) in
    let b = String.sub x (l-2) 2 in
    Int32.add (Int32.mul (Int32.of_string a) 100l) (Int32.of_string b)

let of_int32 x = x
let to_int32 x = x
