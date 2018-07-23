(* A very simple test for the auto-generated schema.capnp *)

let () =
  let open Capnptk in
  let open Schema in
  let open Capnptk.Declarative in

  let bs = Utils.from_stdin () in

  let c = bs |> Utils.decode CodeGeneratorRequest.t in
  (* We will always have the stack structure: 
    (segments, segments, struct) 
  But if an allocator is provided, we will have:
    (allocator, segments, struct)
    *)
  ((c => CodeGeneratorRequest.nodes).(0) => Node.nestedNodes).(0) => Node.NestedNode.id;
  
  ()
