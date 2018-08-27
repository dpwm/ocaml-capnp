module Int64Map = Map.Make(Int64)
open Capnptk.Declarative
open Schema


module StringSet = Set.Make(String)

let modulename_of_filename x = 
  (* We need to transform from lowercase to capitalized
   * and also - -> _.
   *
   * *)
  let b = String.index x '.' in
  let a = try 1+String.rindex x '/' with Not_found -> 0 in
  let base = String.sub x a (b-a) in
  let v = String.capitalize_ascii base in
  v |> String.map (function | '-' -> '_' | v -> v)

let rec rstrip chr str =
  let l = String.length str in
  if str.[l - 1] = chr then
    rstrip chr (String.sub str 0 (l - 1))
  else
    str



let sanitize_name =
  let keywords = StringSet.of_list [
      "field"; "t"; "union"; "union_tag"; "c"; "sg"; "ig"; "type"; "union";
      "object"; "module"; "struct"; "let"; "in"; "open"; "import"; "end";
      "begin"; "done"; "for"; "do"; "true"; "false"; "method"; "Self"; ] in
  fun name ->
    match StringSet.mem (rstrip '_' name) keywords with
    | true -> name ^ "_"
    | false -> name

let node_name ?(capitalize=true) node =
  let p = node => Node.displayNamePrefixLength |> Int32.to_int in
  let s = node => Node.displayName in
  let n = String.length s in
  String.sub s p (n - p) |> (fun x -> 
      if capitalize then String.capitalize_ascii x else x) |> sanitize_name

let rec node_chain get_node node =
  node => Node.scopeId |> function
  | 0L -> [node]
  | id -> node :: (id |> get_node |> node_chain get_node)

let default_combine v (x, l) = 
  let l = List.hd l in
  if l = v then
    Printf.sprintf "Self.%s.t" x
  else
    Printf.sprintf "%s.%s.t" (modulename_of_filename l) x

type state = {
  node : int64 -> Node.t;
  combine : (string * string list) -> string;
  paths : string list Int64Map.t;
  path : string list;
  fmt : Format.formatter;
}

let push_path state x = {state with path = x :: state.path}
let pop_path state = {state with path = List.tl state.path}
let set_filename x state = {state with combine = default_combine x}

let rec show_node_head (state:state) node =
  let open Codegen in

  let paths = state.paths |> Int64Map.add (node => Node.id) state.path in
  let state = {state with paths} in

  let fmt = state.fmt in
  let petname = List.hd state.path in
  let get_node = state.node in


  match node => Node.union with
  | File ->
    fmt |> open_top |>
    comment "This file was generated by capnptk. It is probably not a good idea to edit it." |>
    ctklibs_open |> open_head |> ignore;

    let state = node => Node.nestedNodes |> (state |> Array.fold_left (fun state n -> 
        (n => Node.NestedNode.id |> get_node) |>
        show_node_head (n => Node.NestedNode.name |> String.capitalize_ascii |> sanitize_name |> push_path state) |>
        pop_path
      ))
    in

    state.fmt |> close_module |> ignore;

    state

  | Struct s ->
    let name = petname |> String.capitalize_ascii in
    fmt |> 
    open_module name |>
    structure_type (s=>Node.Struct.dataWordCount) (s=>Node.Struct.pointerCount) |>
    ignore;

    let state = node => Node.nestedNodes |> (state |> Array.fold_left (fun state x -> 
        x => Node.NestedNode.id |> get_node |> 
        show_node_head (x => Node.NestedNode.name |> String.capitalize_ascii |> sanitize_name |> push_path state) |> pop_path)) in


    let state = s => Node.Struct.fields |> (state |> Array.fold_left (fun state x -> 
        match x => Field.union with
        | Group g ->
          g => Field.Group.typeId |> get_node |> show_node_head (x => Field.name |> String.capitalize_ascii |> sanitize_name |> push_path state) |> pop_path
        | _ -> state)) in
    (* we also need to get any fields that represent groups *)

    fmt |> close_module |> ignore;

    state


  | Interface i -> 
    let name = List.hd state.path in
    fmt |> 
    open_module name |> 
    interface_type (i => Node.Interface.methods |> Array.length) (node => Node.id) |>
    ignore;


    let state = node => Node.nestedNodes |> (state |> Array.fold_left (fun state x -> 
        x => Node.NestedNode.id |> get_node |> 
        show_node_head (x => Node.NestedNode.name |> String.capitalize_ascii |> sanitize_name |> push_path state) |> pop_path)) in

    fmt |> close_module |> ignore;
    state

  | Enum e -> 
    let name = node |> node_name in
    let enumerants = 
      e => Node.Enum.enumerants |> Array.map (fun x -> 
          x => Enumerant.name |> String.capitalize_ascii)
    in
    fmt |> 
    open_module name |>
    enum_type enumerants |>
    close_module |> ignore;
    state

  (* We don't define consts until implementation *)
  | Const _ | Annotation _ -> state

let qualified_name get_node node = 
  let (nodes, files) = 
    node |>
    node_chain get_node |>
    List.partition (fun n -> 
        n => Node.union |> function | File -> false | _ -> true)
  in
  let nodes, files = (nodes |> List.map (node_name) |> List.rev |> String.concat ".", files |> List.map (get Node.displayName)) in
  nodes, files


let ocaml_literal value =
  let fmt = Printf.sprintf in
  match value => Value.union with
  | Void -> None
  | Bool v -> if v then Some "true" else None
  | Int8 v | Int16 v | Uint8 v | Uint16 v ->
    if v = 0 then None else Some (fmt "%d" v)
  | Int32 v | Uint32 v ->
    if v = 0l then None else Some (fmt "0x%lxl" v)
  | Int64 v | Uint64 v ->
    if v = 0L then None else Some (fmt "0x%LxL" v)
  | Interface -> None
  | Text t | Data t -> if t = "" then None else Some (fmt "%S" t)
  | Float32 f | Float64 f -> 
    let bits = f |> Int64.bits_of_float in
    if bits = 0L then None else Some (fmt "%F" f)
  | List ap -> 
    ((ap |> c_read_ptr).ptr |> function
      | NullPtr -> None
      | _ -> None) (* failwith "boO List") *)

  | Struct ap -> 
    ((ap |> c_read_ptr).ptr |> function
      | NullPtr -> None
      | _ -> failwith "boO Struct")

  | Enum _ -> None

  | AnyPointer _ -> None

let named_type combine get_node id = 
  id |> get_node |> qualified_name get_node |> combine

let rec ocaml_type combine get_node typ =
  let fmt = Printf.sprintf in
  let named_type id = named_type combine get_node id in
  match typ => Type.union with
  | Void -> "unit"
  | Bool -> "bool"
  | Int8 -> "int"
  | Int16 -> "int"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Uint8 -> "int"
  | Uint16 -> "int"
  | Uint32 -> "int32"
  | Uint64 -> "int64"
  | Float32 -> "float"
  | Float64 -> "float"
  | Text -> "string"
  | Data -> "string"
  | List t -> 
    t => Type.List.elementType |> ocaml_type combine get_node |> fmt "%s array"
  | Enum e -> 
    e => Type.Enum.typeId |> named_type
  | Struct e -> 
    e => Type.Struct.typeId |> named_type |> Printf.sprintf "%s" 
  | Interface i ->
    i => Type.Interface.typeId |> named_type 
  | AnyPointer _ -> "unit c"

let rec capnptk_type combine get_node typ =
  let fmt = Printf.sprintf in
  let named_type id = named_type combine get_node id in
  match typ => Type.union with
  | Void -> "Void"
  | Bool -> "Bool"
  | Int8 -> "Int8"
  | Int16 -> "Int16"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | Uint8 -> "UInt8"
  | Uint16 -> "UInt16"
  | Uint32 -> "UInt32"
  | Uint64 -> "UInt64"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Text -> "Text"
  | Data -> "Data"
  | List t -> 
    t => Type.List.elementType |> capnptk_type combine get_node |> fmt "(List %s)"
  | Enum e -> 
    e => Type.Enum.typeId |> named_type |> fmt "%s"
  | Struct e -> 
    e => Type.Struct.typeId |> named_type |> fmt "%s"
  | Interface i ->
    i => Type.Interface.typeId |> named_type  |> fmt "%s"
  | AnyPointer _ -> "(Ptr Void)"


let capnptk_sizeof typ =
  match typ => Type.union with
  | Int8 | Uint8 -> 8l
  | Int16 | Uint16 | Enum _ -> 16l
  | Int32 | Uint32 | Float32 -> 32l
  | Int64 | Uint64 | Float64 -> 64l
  | _ -> 1l


let field_accessor combine get_node field =
  match field => Field.union with
  | Slot slot when slot => Field.Slot.type_ => Type.union = Void -> 
    None
  | Slot slot -> 
    let default = slot => Field.Slot.defaultValue |> ocaml_literal in
    let default = match default with 
      | Some d -> Printf.sprintf " ~default:%s" d
      | None -> ""
    in
    Some (Printf.sprintf "field t %s%s %ldl" 
            (slot => Field.Slot.type_ |> capnptk_type combine get_node)
            default
            (Int32.mul (slot => Field.Slot.offset) 
               (slot => Field.Slot.type_ |> capnptk_sizeof)))
  | Group group -> 
    Some (Printf.sprintf "group t %s" (group => Field.Group.typeId |> get_node |>
          qualified_name get_node |> combine))

let rec show_node_body (state:state) node =
  let open Codegen in
  let fmt = state.fmt in
  let get_node = state.node in
  let combine = state.combine in
  let name = node_name node in
  let qualified, _ = qualified_name get_node node in
  match node => Node.union with
  | File ->
    fmt |> 
    comment "Now we begin the body. This is what will be referenced by \
             outside scripts, so the first thing we do in each module is alias to the \
             types declared above. " |> open_body |> ignore;

    node => Node.nestedNodes |> Array.iter (fun n -> 
        n => Node.NestedNode.id |> get_node |>
        show_node_body state |> ignore);

    fmt |> close_body |> close_top
  | Struct s ->
    fmt |> 
    open_body_module name |> 
    import_from_head qualified |> ignore;

    (* We should find out if the struct is a union. If it is, we should
     * iterate over all union fields *)

    Node.(node => nestedNodes |> Array.iter (fun x -> x => NestedNode.id |> get_node |> show_node_body state |> ignore));

    s => Node.Struct.fields |> Array.iter (fun x -> 
        match x => Field.union with
        | Group g ->
          g => Field.Group.typeId |> get_node |> show_node_body state |> ignore
        | _ -> ());

    let fields = s => Node.Struct.fields in

    let fields = match s => Node.Struct.discriminantCount with
      | 0 -> fields
      | n -> 
        let base_fields = Array.make ((Array.length fields) - n) fields.(0) in 
        let union_fields = Array.make n fields.(0) in 

        fields |> ((0, 0) |> Array.fold_left (fun (i, j) field ->
            match (field => Field.discriminantValue) with
            | n when n = Field.noDiscriminant -> 
              base_fields.(i) <- field; (i+1, j)
            | _ -> 
              union_fields.(j) <- field; (i, j+1))) |>
        (fun (_, j) ->
           if j <> n then 
             failwith "discriminant count does not match discriminant count"
           else ());

        let tag_offset = s => Node.Struct.discriminantOffset |> Int32.mul 16l in

        union_fields |> Array.map (fun field -> 
            let constructor = field => Field.name |> String.capitalize_ascii in
            let tag = field => Field.discriminantValue in
            let accessor = field |> field_accessor combine get_node in

            let typ = match field => Field.union with
              | Slot slot when slot => Field.Slot.type_ => Type.union = Void ->
                None
              | Slot slot ->
                Some (slot => Field.Slot.type_ |> ocaml_type combine get_node)
              | Group group -> 
                Some (group => Field.Group.typeId |> get_node |> qualified_name get_node |> combine)
            in
            (constructor, typ, tag, accessor)
          ) |> union_block fmt tag_offset;


        base_fields
    in

    fields |> Array.iter (fun field ->
        let open Field in
        match field_accessor combine get_node field with
        | Some accessor ->
          let_statement (field => name |> sanitize_name) accessor fmt |> ignore;
        | None ->
          ()

      );
    fmt |> close_module 

  (* we also need to get any fields that represent groups *)

  | Interface i ->
    let name = node |> node_name in
    fmt |> 
    open_module name |> 
    import_from_head qualified |> ignore;

    Node.(node => nestedNodes |> Array.iter (fun x -> x => NestedNode.id |> get_node |> show_node_body state |> ignore));
    (* There is absolutely no way that anything other than the method that
     * they are assigned to can call the implicit method parameters. This
     * means that we can define them here. They will have the name that
     * capnproto gives them but with $ -> _ *)
    let internal_node internal_nodes = function
      | n when n => Node.scopeId = 0L ->
        let name = n |> node_name |> String.map (function | '$' -> '_' | x -> x) in
        (* By definition this type can have no sub-modules, so it's literally just fields. *)
        fmt |> open_module name |> ignore;

        (match n => Node.union with 
         | Struct s ->
           fmt |> structure_type (s => Node.Struct.dataWordCount) (s =>
                                                                   Node.Struct.pointerCount) |> ignore;
           s => Node.Struct.fields |> Array.iter (fun field -> 
               let open Field in
               match field_accessor combine get_node field with
               | Some accessor -> 
                 let_statement (field => name |> sanitize_name) accessor fmt |> ignore;
               | None -> 
                 ());
         | _ -> 
           failwith "Interface results and parameters must be structs");

        fmt |> close_module |> ignore;
        internal_nodes |> Int64Map.add (n => Node.id) name
      | _ -> internal_nodes
    in

    let internal_map = i => Node.Interface.methods |> Array.fold_left (fun x n ->
        let x = n => Method.paramStructType |> get_node |> internal_node x in
        n => Method.resultStructType |> get_node |> internal_node x
      ) Int64Map.empty in

    let get_node_name x = 
      try (Int64Map.find x internal_map |> Printf.sprintf "%s.t") 
      with Not_found -> 
        get_node x |> qualified_name get_node |> combine
    in

    i => Node.Interface.methods |> Array.iteri (fun n m -> 
        fmt |> interface_method 
          "t"
          (m => Method.paramStructType |> get_node_name) 
          (m => Method.resultStructType |> get_node_name)
          (m => Method.name |> sanitize_name)
          n
        |>
        ignore 

      );
    close_module fmt;

  | Enum _ -> 
    fmt |>
    copy_from_head name qualified

  | Const c -> 
    let name = node_name ~capitalize:false node |> sanitize_name in
    fmt |> let_statement name (c => Node.Const.value |> ocaml_literal |> function | Some x -> x | _ -> failwith "_")

  | Annotation _ -> fmt (* We support no annotations *)

let () =
  let cgr = Capnptk.Utils.(from_stdin () |> decode (CodeGeneratorRequest.t)) in

  let fmt = Format.std_formatter in
  

  let get_node = 
    let nodemap = cgr => CodeGeneratorRequest.nodes |> Int64Map.(
        empty |> Array.fold_left @@ 
        fun m x ->
        m |> add (x => Node.id) x
      )
    in
    fun k -> Int64Map.find k nodemap
  in

  let state = {fmt; node=get_node; paths=Int64Map.empty;  path=[]; combine=default_combine ""} in
  (* Generate the files we need. *)

  let files = CodeGeneratorRequest.(cgr => requestedFiles |> Array.map (fun x -> 
      let buf = Buffer.create 65536 in
      (x => RequestedFile.filename, x => RequestedFile.id, buf, Format.formatter_of_buffer buf, x=>RequestedFile.imports))) in

  let state = files |> (state |> Array.fold_left (
      fun state (filename, fid, _, fmt, _)  -> 
        let state = {state with fmt} in
        fid |> get_node |> show_node_head (filename |> push_path state) |> pop_path
    )) in

  files |> Array.iter (fun (filename, fid, _, fmt, _) -> 
      fid |> get_node |> show_node_body (filename |> push_path {state with fmt} |> set_filename filename) |> ignore;
      Format.pp_print_newline fmt ();
    );

  files |> Array.iter (fun (filename, _, buf, _, _) -> 
      let outname = modulename_of_filename filename |>
                    String.uncapitalize_ascii |>
                    Printf.sprintf "%s.ml" in
      let outchan = Pervasives.open_out outname in
      Buffer.output_buffer outchan buf;
      close_out outchan;
    )
