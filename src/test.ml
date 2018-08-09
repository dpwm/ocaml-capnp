let () =
  let open Capnptkrpc in
  let open Capnptk.Declarative in
  let open Lwt.Infix in
  (* let open Lwt_unix in *)
  let open Test_schema in

  let mainloop () =
    let p = peer () in
    let%lwt _ = peer_connect p in
    peer_loop p |> ignore;



    for%lwt i = 1 to 10000 do
      let fooserver = p |> bootstrap FooServer.t in
      let%lwt _ = fooserver >>=
      csync >>=
      call FooServer.get1 (build FooServer.Get1_Params.t (fun x -> x)) >>=
      csync >>=
      ptrField FooServer.Get1_Results.result >>=
      csync >>=
      call BarServer.get (build BarServer.Get_Params.t (fun x -> x)) >>=
      csync >>=
      fun (x, _, _, _) -> x >>= (fun x -> Lwt_io.printlf "Result: %S" (x => BarServer.Get_Results.result)) in

      let fooserver = p |> bootstrap FooServer.t in
      let%lwt _ = fooserver >>=
      csync >>=
      call FooServer.get2 (build FooServer.Get2_Params.t (fun x -> x)) >>=
      csync >>=
      ptrField FooServer.Get2_Results.result >>=
      csync >>=
      call BarServer.get (build BarServer.Get_Params.t (fun x -> x)) >>=
      csync >>=
        fun (x, _, _, _) -> x >>= (fun x -> Lwt_io.printlf "Result: %S" (x => BarServer.Get_Results.result))
      in

      Lwt.return ()
    done
  in


  Lwt_main.run (mainloop ())
