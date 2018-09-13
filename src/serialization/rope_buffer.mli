type 'a t = {
  pos : int;
  len : int;
  rope : Rope.t;
  spare : Rope.t;
  alloc_size: int;
  result : 'a;
}

val pp : Format.formatter -> _ t -> unit
val alloc : int -> Rope.tree

val make : ?alloc_size:int -> unit -> unit t

val push : 'b -> 'a t -> ('b * 'a) t
val pop : ('b * 'a) t -> ('b * 'a t)

val skip : int -> 'a t -> 'a t
val pos : int -> 'a t -> 'a t

val write_string : string -> 'a t -> 'a t

val write_int64 : int64 -> 'a t -> 'a t
val write_int32 : int32 -> 'a t -> 'a t
val write_int16 : int -> 'a t -> 'a t
val write_int8 : int -> 'a t -> 'a t

val read_int64 : 'a t -> (int64 * 'a) t
val read_int32 : 'a t -> (int32 * 'a) t
val read_int16 : 'a t -> (int * 'a) t
val read_int8 : 'a t -> (int * 'a) t

val to_rope : 'a t -> Rope.t
