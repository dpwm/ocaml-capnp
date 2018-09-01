 
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

  val to_string : t -> string
  (** Unoptimized conversion to string. *)

  val make : int -> t

  val get : t -> int -> char

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

val get_int8 : t -> int -> int
val get_int16 : t -> int -> int
val get_int32 : t -> int -> int32
val get_int64 : t -> int -> int64

val set_int8 : t -> int -> int -> unit
val set_int16 : t -> int -> int -> unit
val set_int32 : t -> int -> int32 -> unit
val set_int64 : t -> int -> int64 -> unit

val split : t -> int -> (t * t)
