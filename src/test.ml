let () =
  let open Capnptkrpc in
  let open Capnptk.Declarative in
  let open Test_schema in
  let open Lwt.Infix in
  let implementation = implement LLServer.t (
    fun x -> x |> 
    declare LLServer.get (fun _ -> 
      set LinkedList.union End |> build LinkedList.t |> Lwt.return
    )) in
  let _server = [| implementation |] |> server in
  let call = build Rpc.Call.t (fun x -> x |> set Rpc.Call.interfaceId 0xb64ac9176c9ee8dfL |> set Rpc.Call.params (build Rpc.Payload.t (fun x -> x))) in
  Printf.printf "0x%Lx\n" (call => Rpc.Call.interfaceId);
  dispatch _server call >>= (fun x -> 
    let x = x |> cast (Ptr Void) (LinkedList.t) in
    x => LinkedList.union |> ignore;
    Lwt.return ()
  ) |> ignore;
  ()
