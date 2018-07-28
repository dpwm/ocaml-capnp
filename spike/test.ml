(* A very simple test for the auto-generated schema.capnp *)

let () =
  let open Capnptk in
  let open Capnptk.Stream in
  let open Rpc in

  let open Capnptk.Declarative in


  Message.(t |> build (fun b ->
    b |> set union (Call Call.(t |> build (fun b -> 
      b |> 
      set sendResultsTo SendResultsTo.(t |> build (fun b ->
        b |> set union Yourself
      )) |> 
      set questionId 23l

    ))
  ))) |> msg |> stream_data |> Utils.to_string |> Printf.printf "%s";

  ()
