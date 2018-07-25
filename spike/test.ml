(* A very simple test for the auto-generated schema.capnp *)

let () =
  let open Capnptk in
  let open Schema in
  let open Capnptk.Declarative in

  let bs = Utils.from_stdin () in

  let fmt = Printf.sprintf in

  let c = bs |> Utils.decode CodeGeneratorRequest.t in

  let new_ = 2 in
  (c => CodeGeneratorRequest.nodes) |> Array.iter (
    fun node -> node => Node.union |> function
      | File -> "FILE!"
      | Struct s -> (s => Node.Struct.fields |> Array.map (get Field.name) |> Array.to_list |> String.concat "; " |> fmt "Struct (%s)")
      | _ -> "_" ;
      |> print_endline
  );
  
  ()
