type ('data, 'a) t = { data : 'data; pos : int;  result : 'a }

(* A very simple (read dangerous) stack based bytestream reader / writer *)
let pop {result=(a, _); _} = a
let popr c = {c with result=(snd c.result)}

type _data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external get_64 : _data -> int -> int64 = "%caml_bigstring_get64"
let map f c = {c with result = f c.result}

let round_up d n = (((n - 1) / d) + 1) * d

let mov d ({pos; _} as c) =  {c with pos = d + pos}
let movw d = mov (8 * d)
let align d c = {c with pos = round_up d c.pos}
let push v c = {c with result = (v, c.result)}
let pop1 ({result = (a, result); _} as c) = a, {c with result}
let pop2 ({result = (b, (a, result)); _} as c) = (a, b), {c with result}
let pop3 ({result = (c, (b, (a, result))); _} as z) = (a, b, c), {z with result}
let pop4 ({result = (d, (c, (b, (a, result)))); _} as z) = (a, b, c, d), {z with result}
let map1 f c = c |> pop1 |> (fun (a, c) -> c |> push (f a))
let map2 f c = c |> pop2 |> (fun (a, c) -> c |> push (f a))
let map3 f c = c |> pop3 |> (fun (a, c) -> c |> push (f a))
let map4 f c = c |> pop4 |> (fun (a, c) -> c |> push (f a))
let setval result c = {c with result}
let setpos pos c = {c with pos}
let id x = x

let read_n read c =
  let rec f (n, c, xs) =
    match n with
    | _ when n > 0 ->
        let x, c = c |> read |> pop1 in
        f (n-1, c, x :: xs)
    | _ -> c |> push (xs |> List.rev |> Array.of_list)
  in

  c |> pop1 |> (fun (n, c) -> (n, c, [])) |> f



let read_int8 c = c |> push c.data.{c.pos} |> mov 1
let read_int16 c = c |> read_int8 |> read_int8 |> map2 (fun (a, b) -> a lor (b lsl 8))
let read_int32 c = c |> read_int16 |> read_int16 |> map2 (fun (a, b) -> Int32.(logor (of_int a) (shift_left (of_int b) 16)))
let read_int64 c = let result = get_64 c.data c.pos in {c with pos=c.pos + 8; result=(result, c.result)}

let write_int8 c = c |> pop1 |> fun (x, c) -> c.data.{c.pos} <- x; c |> mov 1
let write_int16 c = c |> pop1 |> fun (x, c) -> c |> push (x land 0xff) |> write_int8 |> push ((x lsr 8) land 255) |> write_int8
let write_int32 c = c |> pop1 |> fun (x, c) -> 
  c |> 
  push Int32.(logand x 0xffffl |> to_int) |> write_int16  |>
  push Int32.(shift_right_logical x 16 |> to_int) |> write_int16 

let write_int64 c = c |> pop1 |> fun (x, c) -> 
  c |> 
  push Int64.(logand x 0xffffffffL |> to_int32) |> write_int32 |>
  push Int64.(shift_right_logical x 32 |> to_int32) |> write_int32

let write_string c = c |> pop1 |> fun (x, c) ->
  for i = 0 to (String.length x) - 1 do
    c.data.{c.pos + i} <- Char.code x.[i]
  done;
  c |> mov (String.length x)

let withMap read f c = c |> read |> map1 f

let write_bits64u a b c =
  let v, c = pop1 c in
  let x, c = pop1 c in
  c |> push Int64.(
    x |> of_int |> logand (sub (shift_left 1L b) 1L) |> fun x -> shift_left x a |> logor v
  )

let read_bits64u a b c =
  let v, c = pop1 c in
  c |> push Int64.(
    logand (shift_right_logical v a) (sub (shift_left 1L b) 1L) |> to_int
    ) |> push v

let unsigned_to_signed n x =
  let d = 1 lsl n in
  let v = (1 lsl (n - 1)) - 1 in
  if x > v then x - d else x

let read_bits64s a b c =
  c |> read_bits64u a b |> pop2 |> fun ((x, y), c) -> c |> push (unsigned_to_signed b x) |> push y
