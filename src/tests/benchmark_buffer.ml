open Bigarray
(* This takes

real	0m3.109s
user	0m2.806s
sys	0m0.286s

*)

module BigString = struct
  type t = (char, int8_unsigned_elt, c_layout) Array1.t
  let alloc m = Array1.create char c_layout m
  external set_int64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u" [@@noalloc]
  external get_int64 : t -> int -> int64 = "%caml_bigstring_get64u"
end

module Bytes = struct
  type t = Bytes.t
  let alloc m = Bytes.create m
  external set_int64 : t -> int -> int64 -> unit = "%caml_bytes_set64u" [@@noalloc]
  external get_int64 : t -> int -> int64 = "%caml_bytes_get64u"
end

type t =
    | Bytes of Bytes.t
    | Bigstring of BigString.t

let bytes_alloc n = Bytes (Bytes.alloc n)
let bigstring_alloc n = Bigstring (BigString.alloc n)

let set_int64 b =
  match b with
  | Bytes b -> fun n v -> Bytes.set_int64 b n v
  | Bigstring b -> fun n v -> BigString.set_int64 b n v

let get_int64 b =
  match b with
  | Bytes b -> fun n -> Bytes.get_int64 b n
  | Bigstring b -> fun n -> BigString.get_int64 b n

module type CONTAINED = sig
  type t
  val alloc : int -> t
  val set_int64 : t -> int -> int64 -> unit
  val get_int64 : t -> int -> int64
end


let buffer_write_integers m alloc =
  let b = alloc (m*8) in
  let rec f : int -> t -> unit = fun n b ->
    match n with
    | n when n = m -> ()
    | n ->
      set_int64 b (n * 8) (Int64.of_int n);
      f (n + 1) b
  in
  f 0 b;

  let rec g : int -> t -> unit = fun n b ->
    match n with
    | n when n = m -> ()
    | n ->
      let v = get_int64 b (n * 8) in
      if v <> (Int64.of_int n) then failwith "Mismatch";
      g (n+1) b
  in
  g 0 b


let () =
  let m = 100000000 in
  buffer_write_integers m bigstring_alloc
