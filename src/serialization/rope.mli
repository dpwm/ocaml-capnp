type tree =
  | Buffer of Data.t
  | Join of int * int * tree * tree

type path =
  | Root
  | Left of {parent: path; right: tree}
  | Right of {parent: path; left: tree}

type t = {
  path: path;
  tree: tree;
  before: int;
  after: int;
}

val length : tree -> int
(** Get the overall length of this rope *)

val iter : (Data.t -> unit) -> tree -> unit
(** Iterate over the leaf nodes -- for example, to populate a buffer *)

val buffer : Data.t -> tree

val cat : tree -> tree -> tree
(** Append one rope to another. Unlike with strings, this is a very fast operation. *)

val change : tree -> t -> t

val left : t -> t
val right : t -> t
val up : t -> t
val d1 : t -> t
val d2 : t -> t

val of_tree : tree -> t
val to_tree : t -> tree

val full_length : t -> int

val sink_left : t -> t

val sink_right : t -> t

val float_left : t -> t

val float_right : t -> t

val float_top : t -> t

val find : int -> t -> t

val empty : t
