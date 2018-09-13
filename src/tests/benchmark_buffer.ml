open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t
external set_int64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u" [@@noalloc]
external get_int64 : t -> int -> int64 = "%caml_bigstring_get64u"

let buffer_write_integers () =
  let m = 100000000 in
  let b = Array1.create char c_layout (m * 8) in
  (* Array1.fill b '\x00'; *)
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


let () = buffer_write_integers ()
