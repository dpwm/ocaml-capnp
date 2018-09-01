
(* The purpose of this module is to abstract away the operations that are
  * performed. *)

(* We could use bigstringaf, but this relies on c-extensions for some *
   operations we really don't need optimizations for and removes from our "pure
   ocaml" design decision. *)
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

let big_endian (type t) (module EndianOps : EndianOps with type t = t) : t ops =
  let module Swapped = struct
    include EndianOps

    external swap16 : int -> int = "%bswap16"
    external swap32 : int32 -> int32 = "%bswap_int32"
    external swap64 : int64 -> int64 = "%bswap_int64"

    let get_int16 b v = get_int16 b v |> swap16
    let get_int32 b v = get_int32 b v |> swap32
    let get_int64 b v = get_int64 b v |> swap64

    let set_int16 b v x = x |> swap16 |> set_int16 b v
    let set_int32 b v x = x |> swap32 |> set_int32 b v
    let set_int64 b v x = x |> swap64 |> set_int64 b v

  end in
  (module Swapped)

let little_endian (type t) (x : t ops) = x


let native_endian (type t) (x : t ops) =
  if Sys.big_endian then
    big_endian x
  else
    little_endian x

module Bigstring = struct
  open Bigarray
  type t = (char, int8_unsigned_elt, c_layout) Array1.t
  let make n =
    let o = Array1.create char c_layout n in
    Array1.fill o '\000';
    o

  let length = Array1.dim


  let to_string x =
    let len = length x in
    let o = Bytes.create len in
    for i = 0 to (len - 1) do
      Bytes.set o i x.{i};
    done;
    Bytes.unsafe_to_string o

  let get x n = Array1.get x n

  let split x n =
    let len = length x in
    if n >= len then raise (Invalid_argument "split: out of range");
    Array1.sub x 0 n, Array1.sub x n (len - n)

  module type EndianOps' = EndianOps with type t = t

  type t' = t
  module NativeEndianOps : EndianOps' = struct
    type t = t'
    external get_int8 : t -> int -> int = "%caml_ba_ref_1"
    external get_int16 : t -> int -> int = "%caml_bigstring_get16"
    external get_int32 : t -> int -> int32 = "%caml_bigstring_get32"
    external get_int64 : t -> int -> int64 = "%caml_bigstring_get64"

    external set_int8 : t -> int -> int -> unit = "%caml_ba_set_1"
    external set_int16 : t -> int -> int -> unit = "%caml_bigstring_set16"
    external set_int32 : t -> int -> int32 -> unit = "%caml_bigstring_set32"
    external set_int64 : t -> int -> int64 -> unit = "%caml_bigstring_set64"
  end

  module EndianOps = (val (native_endian (module NativeEndianOps : EndianOps')))
  include (EndianOps : EndianOps with type t := t )
end

module String = struct
  type t = string
  let make _ = failwith "String is immutable"

  let length = String.length

  let to_string x = x

  let get x n = x.[n]

  let split _ _ =
    failwith "String is immutable"

  module type EndianOps' = EndianOps with type t = t

  type t' = t
  module NativeEndianOps : EndianOps' = struct
    type t = t'
    external get_int8 : t -> int -> int = "%string_safe_get"
    external get_int16 : t -> int -> int = "%caml_string_get16"
    external get_int32 : t -> int -> int32 = "%caml_string_get32"
    external get_int64 : t -> int -> int64 = "%caml_string_get64"

    let set_int8 _ _ _ = failwith "String is read only"
    let set_int64 = set_int8
    let set_int32 = set_int8
    let set_int16 = set_int8
  end

  module EndianOps = (val (native_endian (module NativeEndianOps : EndianOps')))
  include (EndianOps : EndianOps with type t := t )
end

type 'a rope_segment_ops = (module RopeSegmentOps with type t = 'a)

module type RopeSegment = sig
  include RopeSegmentOps
  val value : t
end

let string = (module String : RopeSegmentOps with type t = string)
let bigstring = (module Bigstring : RopeSegmentOps with type t = Bigstring.t)

type t = (module RopeSegment)

let length (module M : RopeSegment) =
  M.length M.value

let to_string  (module X : RopeSegment) : string =
  X.to_string X.value

let make (type s) (module Ops : RopeSegmentOps with type t = s) n : t =
  (module struct
    include Ops
    let value = Ops.make n
  end)

let from (type s) (module Ops : RopeSegmentOps with type t = s) x : t =
  (module struct
    include Ops
    let value = x
  end)

let get_int8 (module X : RopeSegment) n =
  X.get_int8 X.value n

let get_int16 (module X : RopeSegment) n =
  X.get_int16 X.value n

let get_int32 (module X : RopeSegment) n =
  X.get_int32 X.value n

let get_int64 (module X : RopeSegment) n =
  X.get_int64 X.value n

let set_int8 (module X : RopeSegment) n m =
  X.set_int8 X.value n m

let set_int16 (module X : RopeSegment) n m =
  X.set_int16 X.value n m

let set_int32 (module X : RopeSegment) n m =
  X.set_int32 X.value n m

let set_int64 (module X : RopeSegment) n m =
  X.set_int64 X.value n m

let split (module X : RopeSegment) n =
  let a, b = X.split X.value n in
  (from (module X : RopeSegmentOps with type t = X.t) a,
  from (module X : RopeSegmentOps with type t = X.t) b)
