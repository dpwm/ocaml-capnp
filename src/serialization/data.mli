 
module Bigstring : sig
  type t
end

module type EndianOps = sig
  type t
  val get_int8 : t -> int -> int
  val get_int16 : t -> int -> int
  val get_int32 : t -> int -> int32
  val get_int64 : t -> int -> int64

  val set_int8 : t -> int -> int -> unit
  val set_int16 : t -> int -> int -> unit
  val set_int32 : t -> int -> int32 -> unit
  val set_int64 : t -> int -> int64 -> unit
end

type 't ops = (module EndianOps with type t = 't)

module type RopeSegmentOps = sig
  type t
  include EndianOps with type t := t

  val to_buffer : t -> Buffer.t -> int -> int -> unit
  (* conversion to buffer. *)

  val make : int -> t

  val get : t -> int -> char
  val set : t -> int -> char -> unit

  val length : t -> int

  val split : t -> int -> (t * t)
end

type 'a rope_segment_ops = (module RopeSegmentOps with type t = 'a)

module type RopeSegment = sig
  include RopeSegmentOps
  val value : t
end


val big_endian: 'a ops -> 'a ops
val little_endian : 'a ops -> 'a ops
val native_endian : 'a ops -> 'a ops

type t = (module RopeSegment)

val string : String.t rope_segment_ops
val bigstring : Bigstring.t rope_segment_ops

val length : t -> int


val make : 'a rope_segment_ops -> int -> t
val from : 'a rope_segment_ops -> 'a -> t
val to_string : t -> string
val to_buffer : Buffer.t -> t -> unit
val to_buffer_subrange : Buffer.t -> t -> int -> int -> unit

val get : t -> int -> char
val set : t -> int -> char -> unit

val get_uint8 : t -> int -> int
val get_uint16 : t -> int -> int
val get_int8 : t -> int -> int
val get_int16 : t -> int -> int
val get_int32 : t -> int -> int32
val get_int64 : t -> int -> int64

val set_uint8 : t -> int -> int -> unit
val set_uint16 : t -> int -> int -> unit
val set_int8 : t -> int -> int -> unit
val set_int16 : t -> int -> int -> unit
val set_int32 : t -> int -> int32 -> unit
val set_int64 : t -> int -> int64 -> unit

val of_unsigned : int -> int -> int
val to_unsigned : int -> int -> int

val split : t -> int -> (t * t)
