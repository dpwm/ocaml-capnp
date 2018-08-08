let () =
  let open Capnptkrpc in
  let open Capnptk.Declarative in
  (* let open Lwt.Infix in *)
  (* let open Lwt_unix in *)
  let open Test_schema in

  let mainloop () =
    let p = peer () in
    let%lwt _ = peer_connect p in
    peer_loop p |> ignore;
    let%lwt x = p |> 
    bootstrap FooServer.t |> 
    csync |>
    call FooServer.get (build FooServer.Get_Params.t (fun x -> x)) |> 
    csync |>
    ptrField FooServer.Get_Results.result |> 
    csync |>
    call BarServer.get (build BarServer.Get_Params.t (fun x -> x)) |> 
    fun (x, _, _, _) -> x in

    Lwt_io.printlf "Result: %S" (x => BarServer.Get_Results.result);

  in
  Lwt_main.run (mainloop ())
