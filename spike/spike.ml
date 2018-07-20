open Bigarray
module Int64Map = Map.Make(Int64)

(* let f = Unix.openfile "dump.bin" [O_RDONLY] 0o640 *)

let from_stdin () =
  (* A 1MB buffer should be enough for anybody! *) 
  let max_size = (1 lsl 20) in
  let a = Array1.create int8_unsigned c_layout max_size in
  let buffer = Bytes.create 4096 in

  let rec f n = 
    let r = input stdin buffer 0 4096 in
    match r with
    | 0 -> n
    | r ->
        for i = 0 to r-1 do
          a.{n + i} <- Bytes.get buffer i |> Char.code
        done;
        f (n + r)
  in

  f 0 |> ignore;
  a









(* This is really attempt 1 *)




let make : type a. a Decl.t -> Decl.data_t -> a Decl.cursor = 
  let open Decl in
  fun typ data ->
    let c = {pos=0; data; output=(); dwords=0; pwords=0; frames=[||]} in
    let c = c |> read_int32 in
    let nframes = (c.output |> Int32.to_int) + 1 in
    let frames = Array.make nframes 0 in

    let c = ref c in
    for i = 0 to nframes - 1 do
      c := !c |> read_int32;
      frames.(i) <- !c.output |> Int32.to_int
    done;

    if nframes mod 2 = 0 then
      c := !c |> read_int32;

    let cum = ref 0 in
    for i = 0 to nframes - 1 do
      let n = frames.(i) in
      frames.(i) <- !c.pos + !cum;
      cum := !cum + n * 8;
    done;

    get (Field (typ,0)) {!c with frames}

let rec type_to_string node_map = 
  let open Dschema in
  let open Dschema.Type in 
  let open Decl.Infix in 
  function
  | Int8 -> "int8"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Uint8 -> "uint8"
  | Uint16 -> "uint16"
  | Uint32 -> "uint32"
  | Uint64 -> "uint64"
  | Struct s -> (s =>* Struct.typeId |> (fun x -> Int64Map.find x node_map)) =>* Node.displayName
  | Enum _ -> "enum"
  | Text -> "text"
  | Void -> "void"
  | Bool -> "bool"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Data -> "data"
  | List v -> "list " ^ (v => List.elementType |> get_union |> type_to_string node_map)
  | Interface _ -> "interface"
  | AnyPointer _ -> "anypointer"

(* There are only really a small handful of things that we need to produce. One is a module. *)
module Gen = struct
  type ast =
    | Module of (string * ast array)
    | Let of (string * ast)
    | Union of (string *  ast) array
end
    
let () =
  let open Dschema in
  let open CodeGeneratorRequest in
  let open CapnpVersion in
  let open Decl.Infix in

  let a = from_stdin () in

  (* let a = Unix.map_file Unix.stdin int8_unsigned c_layout false [| -1 |] |> array1_of_genarray in *)
  let cgr = make CodeGeneratorRequest.t a in

  let node_map = cgr =>* nodes |> Array.fold_left (fun node_map node ->
    node_map |> Int64Map.add (node =>* Node.id) node
  ) Int64Map.empty in

  let rec show_node ?(prefix="") node = 

    let displayName = node =>* Node.displayName in
    let typ = node |> Node.get_union |> function
      | File -> "file"
      | Struct _ -> "struct"
      | Enum _ -> "enum"
      | Interface _ -> "interface"
      | Const _ -> "const"
      | Annotation _ -> "annotation"
    in

    Printf.printf "%s%s (%s)\n" prefix displayName typ;

    node |> Node.get_union |> function
      | Struct s -> 
          s =>* Node.Struct.fields |> Array.iter (fun x ->
            let typ, offset = x |> Field.get_union |> function
              | Slot s -> (
                s => Field.Slot.type_ |> Type.get_union |> type_to_string node_map,
                s =>* Field.Slot.offset
              )
              | _ -> ("GROUP", 0l)
            in
            x =>* Field.name |> fun name -> Printf.printf "%s > %s : %s[%lu]\n" prefix name typ offset
          )
      | Enum e ->
          e =>* Node.Enum.enumerants |> Array.iter (fun x ->
            x =>* Enumerant.name |> Printf.printf "%s - %s\n" prefix
          )
      | _ ->
          ()
          ; ;

      

    node =>* Node.nestedNodes |> Array.iter (fun x ->
      let node = node_map |> Int64Map.find_opt (x =>* Node.NestedNode.id) in
      match node with
      | Some node -> 
          let prefix = "  " ^ prefix in
          show_node ~prefix node
      | _ -> ()
    )
  in
  
  cgr =>* requestedFiles |> Array.iter (fun file ->
    let node = node_map |> Int64Map.find (file =>* RequestedFile.id) in
    show_node node 
  ) ;


  ()

