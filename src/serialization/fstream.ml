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

  val read_string : t -> int -> int -> string

  val write_i64 : t -> int -> int64 -> unit
  val write_i32 : t -> int -> int32 -> unit
  val write_u16 : t -> int -> int -> unit
  val write_u8  : t -> int -> int -> unit

  val create : int -> t
  val of_bytes : Bytes.t -> t
  val expand : t -> int -> unit
  val check : t -> int -> unit
  val blit_string : t -> int -> string -> unit
  val length : t -> int
end

module Pos = struct
  type t = {seg : int; off : int}

  let empty = {seg = 0; off = 0}

  let show {seg; off} = Printf.sprintf "{seg=%d; off=%d}" seg off

  let rewind n pos =
    {pos with off = pos.off - n}

  let setoff off pos =
    {pos with off}

  let mov v pos =
    {pos with off = pos.off + v}

  let movw v pos =
    {pos with off = pos.off + 8 * v}
end


module type T = sig
  type data
  type t

  val create : int -> t
  val of_byte_array : Bytes.t array -> t

  val read_i64 : t -> int64
  val read_i32 : t -> int32
  val read_u16 : t -> int
  val read_u8  : t -> int

  val read_i16 : t -> int
  val read_i8  : t -> int

  val read_string : t -> int -> string

  val write_i64 : t -> int64 -> unit
  val write_i32 : t -> int32 -> unit
  val write_u16 : t -> int -> unit
  val write_u8  : t -> int -> unit

  val write_i16 : t -> int -> unit
  val write_i8  : t -> int -> unit

  val posmap : t -> (Pos.t -> Pos.t) -> unit
  val pos : t -> Pos.t
  val setpos : t -> Pos.t -> unit
  val align : t -> int -> unit
  val push : t -> unit
  val pop : t -> unit
  val with_push : (t -> 'a) -> t -> 'a
  val clone : t -> t

  val length : t -> int

  val showpos : t -> string
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

  let read_string x a len = Bytes.sub_string x.bs a len

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
  type t = {mutable pos : Pos.t; mutable stack : Pos.t list; mutable data: data array}

  let of_byte_array bs =
    let data = Array.map S.of_bytes bs in
    {pos=Pos.empty; stack=[]; data}

  let create n =
    let data = [| S.create n |] in
    {pos=Pos.empty; stack=[]; data}

  let check x n =
    try
      let off = x.pos.off + n in
      S.check x.data.(x.pos.seg) off;
      {x.pos with off}
    with End_of_file ->
      begin
        try
          let seg = x.pos.seg + 1 in
          let off = n in 
          S.check x.data.(seg) off;
          {seg; off}
        with
        | Invalid_argument _ ->
          raise End_of_file
      end

  let expand x n =
    (* reserve a contiguous block *)
    try
      S.expand x.data.(x.pos.seg) (x.pos.off + n);
      {x.pos with off = x.pos.off + n}
    with End_of_file -> begin
        let seg = x.pos.seg + 1 in
        try
          S.expand x.data.(seg) (n);
          {seg; off = n}
        with
        | Invalid_argument _ ->
          let len = max (2 * (S.length x.data.(seg-1))) n in
          let b = S.create len in
          x.data <- Array.append x.data [| b |];
          {seg; off=n}

      end


  let check_pos x n f =
    x.pos <- check x n;
    let pos = Pos.rewind n x.pos in
    f x.data.(pos.seg) pos.off

  let expand_pos x n v f =
    x.pos <- expand x n;
    let pos = Pos.rewind n x.pos in
    f x.data.(pos.seg) pos.off v

  let read_i64 x = check_pos x 8 S.read_i64

  let read_i32 x = check_pos x 4 S.read_i32

  let read_u16 x = check_pos x 2 S.read_u16

  let read_u8 x = check_pos x 1 S.read_u8

  let read_i16 x =
    let v = read_u16 x in
    let k = Sys.int_size - 16 in
    (v lsl k) asr k

  let read_i8 x =
    let v = read_u8 x in
    let k = Sys.int_size - 8 in
    (v lsl k) asr k

  let write_i64 x v = expand_pos x 8 v S.write_i64

  let write_i32 x v = expand_pos x 4 v S.write_i32

  let write_u16 x v = expand_pos x 2 v S.write_u16

  let write_u8 x v = expand_pos x 1 v S.write_u8

  let write_i16 = write_u16

  let write_i8 = write_u8

  let pos x = x.pos

  let posmap x f = x.pos <- f x.pos

  let setpos x pos = x.pos <- pos

  let align x n =
    let round_up d n = (((n - 1) / d) + 1) * d in
    let delta = round_up n x.pos.off - x.pos.off in
    let pos = expand x delta in
    x.pos <- pos

  let push x =
    x.stack <- x.pos :: x.stack

  let pop x =
    match x.stack with
    | pos :: xs ->
      x.stack <- xs;
      x.pos <- pos
    | _ -> ()

  let length x =
    x.data |> Array.fold_left (fun x v -> x + S.length v) 0

  let with_push f x =
    push x;
    let o = f x in
    pop x;
    o

  let clone x =
    {x with pos=x.pos}

  let read_string x len = check_pos x len (fun bs off -> S.read_string bs off len)

  let showpos {pos;_} =
    Printf.sprintf "%d:%d" pos.seg pos.off


end

module ByteStream = Make(ByteS)
