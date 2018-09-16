module D = Capnptk.Data
let buffer_write_integers () =
  let m = 100_000_000 in
  let bb = D.(make bigstring (8 * m)) in

  let rec f : int -> D.t -> unit = fun n bb ->
    match n with
    | n when n = m -> ()
    | n ->
      D.set_int64 bb (n * 8) (Int64.of_int n);
      f (n + 1) bb
  in
  f 0 bb;

  let rec g : int -> D.t -> unit = fun n bb ->
    match n with
    | n when n = m -> ()
    | n ->
      let v = D.get_int64 bb (n * 8) in
      if v <> (Int64.of_int n) then failwith "Mismatch";
      g (n+1) bb
  in
  g 0 bb 


let () = buffer_write_integers ()
