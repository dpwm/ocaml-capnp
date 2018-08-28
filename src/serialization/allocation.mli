type t

val length : t -> int
(** Get the overall length of this rope *)

val iter : (Data.t -> unit) -> t -> unit
(** Iterate over the leaf nodes -- for example, to populate a buffer *)

val of_data : Data.t -> t

val to_data : t -> Data.t
(** This is a somewhat slow operation and is avoidable in many cases. *)

val flatten : t -> t
(** Is a convenience function that applies to to_data followed by of_data. *)

val cat : t -> t -> t
(** Append one rope to another. Unlike with strings, this is a very fast operation. *)

val get : t -> int -> char
(** Get individual char. This is suboptimal if many chars will be accessed in a linear fashion. *)

val find : t -> int -> Data.t * int
