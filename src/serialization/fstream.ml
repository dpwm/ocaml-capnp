external get_uint8 : bytes -> int -> int = "%bytes_safe_get"
external get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16"
external get_int32_ne : bytes -> int -> int32 = "%caml_bytes_get32"
external get_int64_ne : bytes -> int -> int64 = "%caml_bytes_get64"
external set_uint8 : bytes -> int -> int -> unit = "%bytes_safe_set"
external set_uint16_ne : bytes -> int -> int -> unit = "%caml_bytes_set16"
external set_int32_ne : bytes -> int -> int32 -> unit = "%caml_bytes_set32"
external set_int64_ne : bytes -> int -> int64 -> unit = "%caml_bytes_set64"
(* external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64" *)

module type S = sig
  type t
  (* These are what must be defined as part of the interface *)

  val read_i64 : t -> int -> int64
  val read_i32 : t -> int -> int32
  val read_u16 : t -> int -> int
  val read_u8  : t -> int -> int

  val write_i64 : t -> int -> int64 -> unit
  val write_i32 : t -> int -> int32 -> unit
  val write_u16 : t -> int -> int -> unit
  val write_u8  : t -> int -> int -> unit

  val create : int -> t
  val of_bytes : Bytes.t -> t
  val expand : t -> int -> unit
  val blit_string : t -> int -> string -> unit
  val length : t -> int
end

module type T = sig
  type data
  type t

  val create : int -> t

  val read_i64 : t -> int64
  val read_i32 : t -> int32
  val read_u16 : t -> int
  val read_u8  : t -> int

  val read_i16 : t -> int
  val read_i8  : t -> int

  val write_i64 : t -> int64 -> unit
  val write_i32 : t -> int32 -> unit
  val write_u16 : t -> int -> unit
  val write_u8  : t -> int -> unit

  val write_i16 : t -> int -> unit
  val write_i8  : t -> int -> unit

  val pos : t -> int
  val setpos : t -> int -> unit
  val align : t -> int -> unit
  val push : t -> unit
  val pop : t -> unit
end

module ByteS = struct
  type t = {
    bs : Bytes.t;
    mutable n : int
  }

  let check x m =
    if m <= x.n
    then ()
    else raise End_of_file

  let expand x m =
    let n' = max x.n m in
    if n' <= Bytes.length x.bs then
      x.n <- n'
    else
      raise End_of_file

  let read_i64 x n =
    check x (n+8);
    get_int64_ne x.bs n

  let read_i32 x n =
    check x (n+4);
    get_int32_ne x.bs n

  let read_u16 x n =
    check x (n+2);
    get_uint16_ne x.bs n

  let read_u8  x n =
    check x (n+1);
    get_uint8 x.bs n

  let write_i64 x n v =
    expand x (n+8);
    set_int64_ne x.bs n v

  let write_i32 x n v =
    expand x (n+4);
    set_int32_ne x.bs n v

  let write_u16 x n v =
    expand x (n+2);
    set_uint16_ne x.bs n v

  let write_u8 x n v =
    expand x (n+1);
    set_uint8 x.bs n v

  let of_bytes bs =
    let n = Bytes.length bs in
    {bs; n}

  let blit_string x n s =
    let l = String.length s in
    expand x (n + l);
    Bytes.blit_string s 0 x.bs n l

  let length {n; _} = n

  let create n =
    {bs=Bytes.create n; n=0}
end

module Make(S : S) = struct
  type data = S.t
  type t = {mutable pos : int; mutable stack : int list; data: data}

  let create n =
    let data = S.create n in
    {pos=0; stack=[]; data}

  let read_i64 x =
    let o = S.read_i64 x.data x.pos in
    x.pos <- x.pos + 8;
    o

  let read_i32 x =
    let o = S.read_i32 x.data x.pos in
    x.pos <- x.pos + 4;
    o

  let read_u16 x =
    let o = S.read_u16 x.data x.pos in
    x.pos <- x.pos + 2;
    o

  let read_i16 x =
    let v = read_u16 x in
    let k = Sys.int_size - 16 in
    (v lsl k) asr k


  let read_u8 x =
    let o = S.read_u8  x.data x.pos in
    x.pos <- x.pos + 1;
    o

  let read_i8 x =
    let v = read_u8 x in
    let k = Sys.int_size - 8 in
    (v lsl k) asr k

  let write_i64 x v =
    S.write_i64 x.data x.pos v;
    x.pos <- x.pos + 8

  let write_i32 x v =
    S.write_i32 x.data x.pos v;
    x.pos <- x.pos + 4

  let write_u16 x v =
    S.write_u16 x.data x.pos v;
    x.pos <- x.pos + 2

  let write_i16 = write_u16

  let write_u8  x v =
    S.write_u8 x.data x.pos v;
    x.pos <- x.pos + 1

  let write_i8 = write_u8

  let pos x = x.pos

  let setpos x pos = x.pos <- pos

  let align x n =
    let round_up d n = (((n - 1) / d) + 1) * d in
    x.pos <- round_up n x.pos

  let push x =
    x.stack <- x.pos :: x.stack

  let pop x =
    match x.stack with
    | pos :: xs ->
      x.stack <- xs;
      x.pos <- pos
    | _ -> ()
end

module ByteStream = Make(ByteS)
