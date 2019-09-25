open Bigarray
open Stream
let from_stdin () =
  (* A 1MB buffer should be enough for anybody! *)
  let max_size = (1 lsl 20) in
  let a = Array1.create int8_unsigned c_layout max_size in
  let buffer = Bytes.create 4096 in

  let rec f n =
    let r = input stdin buffer 0 4096 in
    match r with
    | 0 -> n
    | r ->
      for i = 0 to r-1 do
        a.{n + i} <- Bytes.get buffer i |> Char.code
      done;
      f (n + r)
  in

  let n = f 0 in
  Array1.sub a 0 n


open Declarative
let cursor data =
  {data; pos=0; result=()}


let msg_to_struct t (sections, stream) =
  let pos = ref (stream.pos / 8) in
  let sections = sections |> Array.map (fun x -> let v = !pos in pos := !pos + x; v) in
  {stream; ptr=NullPtr; sections; caps=[||]; impls=empty_impls} |> c_read_ptr |> get (Group (t, None))


let from_bytes t sections data =
  msg_to_struct t (sections, cursor data)



let decode : type a. a sg -> data -> a sgu =
  fun t data ->
  let open Stream in
  cursor data |>
  read_header |>
  pop1 |> 
  msg_to_struct t

let to_bytes x =
  let open Bigarray in
  let n = Array1.dim x in
  let b = Bytes.create n in
  for i = 0 to n-1 do
    Bytes.set b i (Char.chr x.{i})
  done;
  b

let to_string x =
  x |> to_bytes |> Bytes.unsafe_to_string

let struct_to_string : type a. a s c -> string =
  let open Declarative in
  fun x ->
    x |> msg |> msg_data |> to_string

let struct_to_bytes : type a. a s c -> _ =
  let open Declarative in
  fun x ->
    x |> msg |> msg_data

