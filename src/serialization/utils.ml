(*
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
*)


let of_input_channel ic =
  let open Fstream.ByteStream in
  set_binary_mode_in ic true;
  let r' n = really_input_string ic n |> Bytes.unsafe_of_string |> fun x -> of_byte_array [| x |] in
  let b = r' 4 in
  let nseg = read_i32 b |> Int32.to_int |> ((+) 1) in
  let b = r' (4 * nseg) in
  let seglens = Array.init nseg (fun _ -> b |> read_i32 |> Int32.to_int) in
  if nseg mod 2 = 0 then really_input_string ic 4 |> ignore;
  let segs = seglens |> Array.map begin
      fun len -> really_input_string ic (len * 8) |> Bytes.unsafe_of_string
    end in
  of_byte_array segs

let from_stdin () =
  of_input_channel stdin

open Declarative


let msg_to_struct _t (_sections, _stream) =
  failwith "not implemented"
  (*
  let pos = ref (stream.pos / 8) in
  let sections = sections |> Array.map (fun x -> let v = !pos in pos := !pos + x; v) in
  failwith "foo"
  {stream; ptr=NullPtr; sections; caps=[||]; impls=empty_impls} |> c_read_ptr |> get (Group (t, None)) *)


(* let from_bytes t sections data =
  msg_to_struct t (sections, cursor data)
*)

let show_pos : ByteStream.t -> string = fun bs ->
  let p = ByteStream.pos bs in
  Printf.sprintf "%d:%d" p.seg p.off


let decode : type a. a sg -> ByteStream.t -> a sgu =
  fun t stream ->
  let c = {stream; ptr=NullPtr; caps=[||]; impls=(0, [])} in
  let c = c_read_ptr c in
  show_pos c.stream |> print_endline;
  c |> get (Group (t, None))
  (* 
  let open Stream in
  cursor data |>
  read_header |>
  pop1 |>
  msg_to_struct t
     *)

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

(*
let struct_to_string : type a. a s c -> string =
  let open Declarative in
  fun x ->
    x |> msg |> msg_data |> to_string

let struct_to_bytes : type a. a s c -> _ =
  let open Declarative in
  fun x ->
    x |> msg |> msg_data

   *)

let struct_to_bytes : type a. a s c -> _ =
  fun _ ->
  failwith "Not implemented"
