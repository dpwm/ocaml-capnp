module Types = struct
  type 'a _structure = Int64.t

  type 'a t =
    | Void : unit t
    | Bool : bool t
    | UInt64 : Unsigned.Int64.t t
    | UInt32 : Unsigned.Int32.t t
    | UInt16 : int t
    | UInt8 : int t
    | Int64 : Int64.t t
    | Int32 : Int32.t t
    | Int16 : int t
    | Int8 : int t
    | Float32: float t
    | Float64: float t
    | Text : string t
    | Data : string t
    | List : 'a t -> 'a listproxy t
    | Struct : Unsigned.Int64.t -> ('z _structure) t
    | Defer : (unit -> 'a t) -> unit t

  and 'a listproxy = ListProxy : 'a listproxy

  let i8 = Int8
  let i16 = Int16
  let i32 = Int32
  let i64 = Int64

  let u64 = UInt64
  let u32 = UInt32
  let u16 = UInt16
  let u8 = UInt8

  let list x = List x
  let defer f = Defer f;;

  type 'a structure = 'a _structure t;;
  type ('a, 'b) field = { struct_type: 'a structure; typ : 'b t; offset : int; default : 'b option}

  let field ?default struct_type typ offset = {struct_type;typ;offset;default}
  let structure id = Struct id;;

end;;

module Write = struct
  let write_int8 buf offset v =
    Bytes.set buf offset (Char.chr (v land 255));;
  let write_int16 buf offset v =
    write_int8 buf offset v;
    write_int8 buf (offset + 1) (v lsr 8);
  ;;
end;;


module Read = struct
  type ptr = 
    | StructPtr of int * int
    | ListPtr of int * int
    | FarPtr of int
  ;;

  let sl64 n x = Int64.shift_left x n;;
  let sl32 n x = Int32.shift_left x n;;
  let sli n x = x lsl n;;

  let read_int8 buf i = Bytes.get buf i |> Char.code;;
  let byte_f = read_int8;;

  let read_int64 buf offset =
    let f i = byte_f buf (offset + i) |> Int64.of_int |> sl64 (8 * i) in
    Int64.logor 
    (Int64.logor
    (Int64.logor (f 0) (f 1))
    (Int64.logor (f 2) (f 3)))
    (Int64.logor 
    (Int64.logor (f 4) (f 5))
    (Int64.logor (f 6) (f 7)))
  ;;

  let read_int32 buf offset =
    let f i = byte_f buf (offset + i) |> Int32.of_int |> sl32 (8 * i) in
    Int32.logor
    (Int32.logor (f 0) (f 1))
    (Int32.logor (f 2) (f 3))
  ;;

  let read_int16 buf offset =
    let f i = byte_f buf (offset + i) |> sli (8 * i) in
    (f 0) lor (f 1)
  ;;

  let rel u = Int32.shift_right_logical u 2 |> Int32.to_int;;

  let struct_ptr v = 
    Printf.printf "%ld\n" v;
    let d = Int32.logand v 0xffffl |> Int32.to_int in
    let p = Int32.shift_right_logical v 16 |> Int32.to_int in
    StructPtr (d, p)
  ;;


  let list_ptr v =
    let t = Int32.logand 3l v |> Int32.to_int in
    let n = Int32.shift_right_logical v 3 |> Int32.to_int in
    ListPtr (t, n)
  ;;

  let read_ptr (buf, offset, x) =
    let u = read_int32 buf offset in
    let v = read_int32 buf (offset + 4) in
    
    (buf, offset + 8, match (Int32.logand u 3l) with
    | 0l -> (x, (rel u, struct_ptr v))
    | 1l -> (x, (rel u, list_ptr v))
    | _ -> failwith "far pointers not implemented"
    )
  ;;

  let read_frame (buf, offset, ()) = 
    let n = read_int32 buf offset |> Int32.to_int in

    let segment_lengths = Array.make (n + 1) 0 in 

    for i = 0 to n do
      let o = offset + 4 * (i + 1) in
      segment_lengths.(i) <- Int32.to_int (read_int32 buf o);
    done;

    let offset = offset + (n + 2) * 4 in
    
    (buf, offset, segment_lengths)
  ;;


end;;

open Types


module Date = struct
  type t;;
  let t : t structure = structure 0xef29c66fa74a8c93L;;

  let year = field ~default:2017 t i16 0;;
  let month = field t i8 16;;
  let day = field t i8 24;;
end;;

module Person = struct
  type t;;
  let t : t structure = structure 0xef29c66fa74a8c93L;;

  let dob = field t Date.t 0;;
  let friends = field t (List t) 1;;
end;;


let apply_default : type a. ('b, a) field -> a -> a =
  fun f ->
  match (f.default, f.typ) with
  | (None, _) -> fun z -> z
  | (Some x, Int16) -> 
      fun z -> x lxor z
  | (Some x, Int8) -> 
      fun z -> x lxor z
  | (Some x, Int32) ->
      fun z -> Int32.logxor x z
  | (Some x, _) ->
      fun z -> x
;;


let get : type a. ('b, a) field -> (bytes * int) -> a =
  fun f (bs, offset) -> 
    (* Read the *)
    match f.typ with
    | Int16 -> Read.read_int16 bs (offset + f.offset lsr 3) |> apply_default f
    | Int8 -> Read.read_int8 bs (offset + f.offset lsr 3) |> apply_default f
;;

let set : type a. ('b, a) field -> (bytes * int) -> a -> unit =
  fun f (bs, offset) v -> 
    let v = apply_default f v in
    match f.typ with
    | Int16 -> Write.write_int16 bs (offset + f.offset lsr 3) v
    | Int8 -> Write.write_int8 bs (offset + f.offset lsr 3) v
;;

let () =
  let buf = "\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x05\x00\x0c\x03\x00\x00\x00\x00" |> Bytes.of_string in
  get Date.year (buf,16) |> Printf.printf "%d\n"
;;
