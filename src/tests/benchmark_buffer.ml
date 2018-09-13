open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t
external set_int64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u" [@@noalloc]
external get_int64 : t -> int -> int64 = "%caml_bigstring_get64u"

type 'a iface = {
  get_int64 : t -> int -> int64;
  set_int64 : t -> int -> int64 -> unit;
}

type 'a store = {
  iface : 'a iface;
  value : 'a
}

let bigstring_iface = {get_int64; set_int64}

type box = Box of t

let buffer_write_integers () =
  let m = 100000000 in
  let bb = {value = (Array1.create char c_layout (8 * m)); iface=bigstring_iface} in
  let rec f : int -> t iface -> t -> unit = fun n iface value ->
    match n with
    | n when n = m -> ()
    | n ->
      iface.set_int64 value (n * 8) (Int64.of_int n);
      f (n + 1) iface value
  in
  f 0 bb.iface bb.value;

  let rec g : int -> t iface -> t ->  unit = fun n iface value ->
    match n with
    | n when n = m -> ()
    | n ->
      let v = iface.get_int64 value (n * 8) in
      if v <> (Int64.of_int n) then failwith "Mismatch";
      g (n+1) iface value
  in
  g 0 bb.iface bb.value


let () = buffer_write_integers ()
