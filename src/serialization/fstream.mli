module type S = sig
  type t

  val read_i64 : t -> int -> int64
  val read_i32 : t -> int -> int32
  val read_u16 : t -> int -> int
  val read_u8  : t -> int -> int

  val write_i64 : t -> int -> int64 -> unit
  val write_i32 : t -> int -> int32 -> unit
  val write_u16 : t -> int -> int -> unit
  val write_u8  : t -> int -> int -> unit

  val create: int -> t
  val of_bytes : Bytes.t -> t
  val expand : t -> int -> unit
  val blit_string : t -> int -> string -> unit
  val length : t -> int
end

module ByteS : S

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

module Make (S:S) : T with type data = S.t

module ByteStream : T with type data = ByteS.t
