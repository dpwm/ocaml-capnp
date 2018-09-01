type t = {
  pos : int;
  len : int;
  rope : Rope.t;
  spare : Rope.t;
  alloc_size: int;
}

val alloc : int -> Rope.tree

val make : ?alloc_size:int -> unit -> t

val skip : int -> t -> t

val write_string : string -> t -> t

