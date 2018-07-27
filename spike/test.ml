(* A very simple test for the auto-generated schema.capnp *)

let () =
  let open Capnptk in
  let open Myschema in
  let open Capnptk.Declarative in
  let open Bigarray in 

  (* let bs = Utils.from_stdin () in *)

  let fmt = Printf.sprintf in

  let test_person = Person.(
    t |> build (fun x -> 
    x |> 
    set name "Test Person" |>
    set dob Date.(t |> build (fun x -> 
      x |> set year 2000 |> set month 1 |> set day 1)
    ) |>
    set friends [| t |> build (fun x -> 
      x |> 
      set name "Test Friend" |>
      set dob Date.(t |> build (fun x -> x |> set year 1999 |> set month 1 |> set day 1))) |]

  )) |> msg |> stream_data;
  in

  
  Utils.to_string test_person |> Printf.printf "%s";
  (* Person.(test_person => name) |> print_endline; *)

  ()
