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
  let call = make_call LLServer.get (build LLServer.Get_Params.t (fun x -> x))  in

  call |> msg |> stream_data |> Capnptk.Utils.to_string |> Printf.printf "%s";

  dispatch _server call >>= (fun x -> 
    let x = x |> cast_struct (Ptr Void) (LinkedList.t) in
    x => LinkedList.union |> ignore;
    Lwt.return ()
  ) |> ignore;
  ()
