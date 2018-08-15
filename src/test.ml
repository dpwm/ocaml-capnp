let () =
  let open Capnptkrpc in
  let open Capnptk.Declarative in
  (* let open Lwt.Infix in *)
  let open Test_schema in
  (* let open Lwt_unix in *)
  (*

  let mainloop () =
    let p = peer () in
    let%lwt _ = peer_connect p in
    peer_loop p |> ignore;



    for%lwt i = 1 to 10 do
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
  *)
  (* Lwt_main.run (mainloop ()) *)

  (* In any case, the peer does need to know about the things that have been
   * created. But it can have them with refcount 0, which means that we can
   * easily gc them ourselves. *)
  
  (* The implementations should really be decoupled from the peer. Because we
   * might even want to share implementations with mutable state between peers.
   * But there are no clean ways to do this decoupling that don't effectively
   * downcast.
   * *)
  let barserver name = 
    implement BarServer.t (fun x -> x |>
      declare BarServer.get (fun _ -> 
        build BarServer.Get_Results.t (fun x -> x |> set BarServer.Get_Results.result name) |> Lwt.return)
  ) in

  let fooserver = FooServer.(
    implement t (fun x -> x |>
    declare get1 (fun _ -> Get1_Results.(build t (
      fun b -> b |> set result (barserver "BarServer(Get1)")
    ) |> Lwt.return)))) in

  let p = peer ~bootstrap:fooserver () in
  p |> ignore;

  ()
