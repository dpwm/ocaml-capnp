module D = Capnptk.Data
let buffer_write_integers () =
  let m = 100000000 in
  let bb = Capnptk.Data.(make bigstring (8 * m)) in
  let rec f : int -> unit = fun n ->
    match n with
    | n when n = m -> ()
    | n ->
      D.set_int64 bb (n * 8) (Int64.of_int n);
      f (n + 1)
  in
  f 0;

  let rec g : int -> unit = fun n ->
    match n with
    | n when n = m -> ()
    | n ->
      let v = D.get_int64 bb (n * 8) in
      if v <> (Int64.of_int n) then failwith "Mismatch";
      g (n+1)
  in
  g 0


let () = buffer_write_integers ()
