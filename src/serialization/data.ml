
(* The purpose of this module is to abstract away the operations that are
  * performed. *)

(* We could use bigstringaf, but this relies on c-extensions for some *
operations we really don't need optimizations for and removes from our "pure
ocaml" design decision. *)

open Bigarray
type t = (char, int8_unsigned_elt, c_layout) Array1.t
let make n = Array1.create char c_layout n
let length = Array1.dim

let of_string x =
  let len = String.length x in
  let o = make len in
  for i = 0 to (len - 1) do
    o.{i} <- x.[i];
  done;
  o

let to_string x =
  let len = length x in
  let o = Bytes.create len in
  for i = 0 to (len - 1) do
    Bytes.set o i x.{i};
  done;
  Bytes.unsafe_to_string o

let get x n = Array1.get x n

module type Ops = sig
  val get_int8 : t -> int -> int
  val get_int16 : t -> int -> int
  val get_int32 : t -> int -> int32
  val get_int64 : t -> int -> int64

  val set_int8 : t -> int -> int -> unit
  val set_int16 : t -> int -> int -> unit
  val set_int32 : t -> int -> int32 -> unit
  val set_int64 : t -> int -> int64 -> unit
end


module NativeOps : Ops = struct
  external get_int8 : t -> int -> int = "%caml_ba_ref_1"
  external get_int16 : t -> int -> int = "%caml_bigstring_get16"
  external get_int32 : t -> int -> int32 = "%caml_bigstring_get32"
  external get_int64 : t -> int -> int64 = "%caml_bigstring_get64"

  external set_int8 : t -> int -> int -> unit = "%caml_ba_set_1"
  external set_int16 : t -> int -> int -> unit = "%caml_bigstring_set16"
  external set_int32 : t -> int -> int32 -> unit = "%caml_bigstring_set32"
  external set_int64 : t -> int -> int64 -> unit = "%caml_bigstring_set64"
end

module SwappedOps : Ops = struct
  include NativeOps
  external swap16 : int -> int = "%bswap16"
  external swap32 : int32 -> int32 = "%bswap_int32"
  external swap64 : int64 -> int64 = "%bswap_int64"

  let get_int16 b v = get_int16 b v |> swap16
  let get_int32 b v = get_int32 b v |> swap32
  let get_int64 b v = get_int64 b v |> swap64

  let set_int16 b v x = x |> swap16 |> set_int16 b v
  let set_int32 b v x = x |> swap32 |> set_int32 b v
  let set_int64 b v x = x |> swap64 |> set_int64 b v
end

include (val
          (if Sys.big_endian then
              (module SwappedOps)
            else (module NativeOps)) : Ops)

module Buffer = struct
  type t' = t
  type t = {
    mutable pos : int;
    data : t' }

  let make n = {
    data = make n;
    pos = 0; }

  let add xs x =
    let len = length x in
    Array1.blit x (Array1.sub xs.data xs.pos len);
    xs.pos <- xs.pos + len

  let contents xs =
    xs.data
end
