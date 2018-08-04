open Test_schema
open Capnptk.Declarative

let build = build LinkedList.t

let rec build_list n =
  build (fun x ->
    x |> set LinkedList.payload (Int64.of_int n) |>
    (match n with 
    | 0 -> set LinkedList.union End
    | n -> set LinkedList.union (Cons (build_list (n-1)))))

(* let foo = 
  build_list 10000 |> msg |> stream_data |> Capnptk.Utils.to_string |> Printf.printf "%s";
  ()
  *)

let foo () = 
  let rec follow node =
    match node => LinkedList.union with
    | Cons x -> follow x;
    | End -> ();
  in
  let x = Capnptk.Utils.from_stdin () |> 
  Capnptk.Utils.decode LinkedList.t in
  for _ = 0 to 1000 do
    follow x;
  done

module Test_schema = Test_schema_.Make(Capnp.BytesMessage)

let bar () =
  let rec follow node =
    match Test_schema.Reader.LinkedList.get node with
    | Cons node -> follow node
    | End -> ()
  in

  let root_struct = Capnp_unix.IO.read_single_message_from_channel ~compression:`None stdin |> (function | Some x -> x | None -> failwith "no msg") |> Test_schema.Reader.LinkedList.of_message in

  for _ = 0 to 1000 do
    follow root_struct;
  done


let () = bar ()
