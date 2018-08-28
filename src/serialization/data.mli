open Bigarray
type t = (char, int8_unsigned_elt, c_layout) Array1.t

val of_string : string -> t
(** Unoptimized conversion of string to bigstring. *)

val to_string : t -> string
(** Unoptimized conversion of bigstring to string. *)

val get : t -> int -> char
(** Unoptimized getting of an individual char. *)

val length : t -> int

val get_int8 : t -> int -> int
val get_int16 : t -> int -> int
val get_int32 : t -> int -> int32
val get_int64 : t -> int -> int64

val set_int8 : t -> int -> int -> unit
val set_int16 : t -> int -> int -> unit
val set_int32 : t -> int -> int32 -> unit
val set_int64 : t -> int -> int64 -> unit

module Buffer : sig
  type t' = t
  type t

  val make : int -> t
  (** Create a new buffer of size n. Note that unlike Buffer from the stdlib,
    * this does not reallocate. *)

  val add : t -> t' -> unit
  (** Add to the end of the buffer *)

  val contents : t -> t'
  (** Extract the contents of the buffer *)
end
