let buffer_write_integers () =
  let m = 100000000 in
  let module RB = Capnptk.RopeBuffer in
  let r = RB.make () in
  let rec f : int -> 'a RB.t -> 'a RB.t = fun n r ->
    match n with
    | n when n = m -> r
    | n ->
      let r = RB.write_int64 (Int64.of_int n) r in
      f (n + 1) r
  in
  let x = f 0 r |> RB.pos 0 in

  let rec g : int -> 'a RB.t -> unit = fun n r ->
    match n with
    | n when n = m -> ()
    | n ->
      let v, r = r |> RB.read_int64 |> RB.pop in
      if v <> (Int64.of_int n) then failwith "Mismatch";
      g (n+1) r
  in
  g 0 x


let () = buffer_write_integers ()
