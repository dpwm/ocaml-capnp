let () =
  let open Schema in
  let open Capnptk.Declarative in
  let c = Capnptk.Utils.(from_stdin () |> decode (CodeGeneratorRequest.t)) => CodeGeneratorRequest.nodes |> Array.iter (
    fun node -> node => Node.displayName |> print_endline
  ) in 
  c
