type field_type = 
  | Slot of int32 * string * string
  | Group of ast
and
field = {
  name : string;
  typ : field_type;
  uniont : int option;
}
and
ast =
  | File of (string * ast array)
  | Enum of (string * string *  string array)
  | Interface of (string * int64)
  | Const of string
  | Annotation of string
  | Struct of (string * string * int * int * int32 option * field array * ast array)

let flip f a b = f b a

type output_builder = {
  lines : (int * string) list;
  level : int;
}


let add_line line lines = {
  lines with
  lines = (lines.level, line) :: lines.lines;
}

let empty_lines = { lines=[]; level=0 }

let indent x = {x with level = 1 + x.level}
let unindent x = {
  x with level = -1 + x.level;
  lines = match x.lines with (_, "") :: xs -> xs | xs -> xs ;
}

let sanitize_name = function
  | "inherit" -> "inherit_"
  | "type" -> "type_"
  | "class" -> "class_"
  | "struct" -> "struct_"
  | "field" -> "field_"
  | "union" -> "union_"
  | "group" -> "group_"
  | "ptr" -> "ptr_"
  | "t" -> "t_"
  | "ug" -> "ug_"
  | "union_tag" -> "union_tag_"
  | "Types" -> "Types_"
  | "Defns" -> "Defns_"
  | x -> x


module Int64Map = Map.Make(Int64)


let rec process_node node_map node = 
  let open Decl in
  let open Decl.Infix in
  let open Dschema in


  let get_name c =
    let open Decl.Infix in
    let open Dschema in
    let name = c =>* Node.displayName in
    let prefix = c =>* Node.displayNamePrefixLength |> Int32.to_int in
    String.sub name prefix  (String.length name - prefix) |> String.capitalize_ascii |> sanitize_name
  in


  let get_full_name c =
    let open Decl.Infix in
    let open Dschema in


    let rec f x = 
      let parent = node_map |> Int64Map.find_opt (x =>* Node.scopeId) in
      match parent with
      | Some parent -> get_name parent :: f parent
      | None -> []
    in

    String.concat "." (List.tl (List.rev ((get_name c) :: f c)))
  in


  let rec type_to_string node_map = 
    let open Dschema in
    let open Dschema.Type in 
    let open Decl.Infix in 
    function
    | Int8 -> "Int8"
    | Int16 -> "Int16"
    | Int32 -> "Int32"
    | Int64 -> "Int64"
    | Uint8 -> "UInt8"
    | Uint16 -> "UInt16"
    | Uint32 -> "UInt32"
    | Uint64 -> "UInt64"
    | Struct s -> (s =>* Struct.typeId |> (fun x -> Int64Map.find x node_map)) |> get_full_name |> Printf.sprintf "(ptr Types.%s.t)"
    | Enum s -> (s =>* Enum.typeId |> (fun x -> Int64Map.find x node_map)) |> get_full_name |> Printf.sprintf "Types.%s.t"
    | Void -> "Void"
    | Bool -> "Bool"
    | Float32 -> "Float32"
    | Float64 -> "Float64"
    | Data -> "Data"
    | Text -> "Text"
    | List v -> Printf.sprintf "(List %s)" (v => List.elementType |> get_union |> type_to_string node_map)
    | Interface _ -> "Interface"
    | AnyPointer _ -> "(Ptr Void)"
  in

  let elemsize = 
    let open Dschema.Type in
    function
    | Uint8 | Int8 -> 8l
    | Uint16 | Int16 | Enum _ -> 16l
    | Uint32 | Int32 | Float32 -> 32l
    | Uint64 | Int64 | Float64 -> 64l
    | _ -> 1l
  in


  let rec type_to_camltype_string node_map = 
    let open Dschema in
    let open Dschema.Type in 
    let open Decl.Infix in 
    function
    | Int8 -> "int"
    | Int16 -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Uint8 -> "int"
    | Uint16 -> "int"
    | Uint32 -> "int32"
    | Uint64 -> "int64"
    | Struct s -> (s =>* Struct.typeId |> (fun x -> Int64Map.find x node_map)) |> get_full_name |> Printf.sprintf "Types.%s.t s c"
    | Enum s -> (s =>* Enum.typeId |> (fun x -> Int64Map.find x node_map)) |> get_full_name |> Printf.sprintf "Types.%s.t"
    | Void -> "unit"
    | Bool -> "bool"
    | Float32 -> "float"
    | Float64 -> "float"
    | Data -> "string"
    | Text -> "string"
    | List v -> Printf.sprintf "%s array" (v => List.elementType |> get_union |> type_to_camltype_string node_map)
    | Interface _ -> "Interface"
    | AnyPointer _ -> "unit c"
  in






  let nested = node =>* Node.nestedNodes |> Array.map (fun x ->
    let node = node_map |> Int64Map.find_opt (x =>* Node.NestedNode.id) in
    match node with
    | Some node -> 
        process_node node_map node
    | _ ->
        failwith "No match"
  )
  in


  let displayName = node =>* Node.displayName in

  node |> Node.get_union |> function
    | File -> File (displayName, nested)
    | Struct s -> 
        let discriminantOffset = if s=>* Node.Struct.discriminantCount > 0 then Some (Int32.mul 16l (s =>*  Node.Struct.discriminantOffset)) else None in
        let dwords = s =>* Node.Struct.dataWordCount in
        let pwords = s =>* Node.Struct.pointerCount in
        let fields = s =>* Node.Struct.fields |> Array.map (fun field -> 
          let name = field =>* Field.name in
          let uniont = field =>* Field.discriminantValue |> function | 0 -> None | n -> Some (n lxor 65535) in

          Field.get_union field |> function
            | Slot s -> { uniont; name; typ=Slot (
              Int32.(mul (s =>* Field.Slot.offset) (s => Field.Slot.type_ |> Type.get_union |> elemsize)),
              s=> Field.Slot.type_ |> Type.get_union |> type_to_string node_map,
              s=> Field.Slot.type_ |> Type.get_union |> type_to_camltype_string node_map) }
            | Group g -> { uniont; name; typ=Group (node_map |> Int64Map.find (g =>* Field.Group.typeId) |> process_node node_map)}

          ) in
        Struct (get_name node, get_full_name node, dwords, pwords, discriminantOffset, fields, nested)
    | Enum s -> Enum (get_name node, get_full_name node, s=>*Node.Enum.enumerants |> Array.map (fun f -> f =>* Enumerant.name))
    | Interface _ ->
        Interface (get_name node, 0xdeadbeefL)
    | Const _ -> Const (get_name node)
    | Annotation _ -> Annotation (get_name node)

  let f = Printf.sprintf

  let rec show (lines, bodylines) node = match node with
    | File (name, ast) ->
        let bodylines = 
          bodylines |>
          add_line "" |>
          add_line "(* The rest of the file contains the real declarations. *)" |>

          add_line "module Decls = struct" |>
          indent |>
          add_line "open Capnptk.Declarative" |>
          add_line ""
        in

        let lines = lines |>
        add_line (Printf.sprintf "(* %s *)\n" name) |> 
        add_line "(* This file was auto-generated from a capnproto schema." |>
        add_line "   You should not need to edit this file!                *)" |>
        add_line "" |> 
        add_line "(* The Types module pre-defines all the types used. " |>
        add_line "   This simplifies implementation by acting as forward definitions.  *)" |>
        add_line "" |> 
        add_line "module Types = struct" |>
        indent |>
        add_line "" |> 
        add_line "open Capnptk.Declarative" |>
        add_line "" in
        
        let lines, bodylines = Array.fold_left show (lines, bodylines) ast in

        (lines |> unindent |> add_line "end",
         bodylines |> unindent |> add_line "end" |> add_line "" |> add_line "include Decls")
         

    | Struct (name, fullname, dwords, pwords, discriminantOffset, fields, submodules) ->

        let lines = lines |>
        add_line (f "module %s = struct" name) |>
        indent |> 
        add_line "type t" |>
        add_line (f "let t : t sg = sg %d %d" dwords pwords) |>
        add_line "" in

        let bodylines = 
          bodylines |>
          add_line (f "module %s = struct" name) |>
          indent  |>
          add_line (f "type t = Types.%s.t" fullname) |>
          add_line (f "let t = Types.%s.t" fullname)
        in

        let lines, bodylines = submodules |> Array.fold_left show (lines, bodylines) in

        let lines, bodylines = fields |> Array.fold_left (fun x field ->
          match field.typ with
          | Group n ->
              show x n
          | _ -> x
        ) (lines, bodylines) in


        let bodylines = bodylines |>
          add_line "" |>
          add_line "(* Field definitions *)"
        in

        let bodylines, union_members = fields |> Array.fold_left (fun (bodylines,union_members) field ->
          let name = sanitize_name field.name in
          match field.typ with
          | _ when field.uniont <> None -> bodylines, field::union_members
          | Group (Struct (_, fullname, _, _, _, _, _)) ->
            bodylines |>
            add_line (f "let %s = group t (ptr Types.%s.t)" name fullname),
            union_members
          | Slot (offset, typ, _) -> bodylines |> 
            add_line (f "let %s = field t %s %lul" name typ offset),
            union_members
        ) (bodylines, []) in

        let bodylines = if union_members <> [] then begin
          let bodylines = bodylines |> add_line "" |> 
          add_line "(* Unnamed union *)" |>
          add_line "" |>
          add_line "type union =" |>
          indent
          in

          let bodylines = 
            union_members |> List.rev |> List.fold_left (fun bodylines field ->
              let name = String.capitalize_ascii field.name in
              match field.typ with
              | _ when None = field.uniont -> bodylines
              | Group (Struct (_, fullname, _, _, _, _, _)) ->
                  bodylines |>
                add_line (f "| %s of Types.%s.t s c" name fullname)
              | Slot (offset, "Void",_) -> bodylines |> 
                add_line (f "| %s" name)
              | Slot (offset, _, typ) -> bodylines |> 
                add_line (f "| %s of %s" name typ)
            ) bodylines in

          let offset = match discriminantOffset with | Some offset -> offset | None -> failwith "discriminant must have offset" in

          bodylines |> unindent |> 
          add_line "let union =" |> indent |>
          add_line (f "let union_tag = field t UInt16 %lul in" offset) |>
          add_line "let f c =" |> indent |>
          add_line (f "match c |> get union_tag with") |> indent |>
          (fun bodylines -> union_members |> List.rev |> List.fold_left (fun bodylines field ->
            let name = String.capitalize_ascii field.name in
            bodylines |>
            match field.uniont, field.typ with
            | Some n, Group (Struct (_, fullname, _, _, _, _, _)) ->
              add_line (f "| %d -> %s (c |> get (group t @@ Ptr Types.%s.t))" n name fullname)
            | Some n, Slot (offset, "Void",_) -> 
              add_line (f "| %d -> %s" n name)
            | Some n, Slot (offset, typ, _) -> 
              add_line (f "| %d -> %s (get (field t %s %lul) c)" n name typ offset)
          ) bodylines) |>
          add_line "| n -> failwith @@ Printf.sprintf \"Invalid union tag: %d\" n" |>
          unindent |>
          unindent |> 
          add_line "in let g b = function" |> indent |>
          (fun bodylines -> union_members |> List.rev |> List.fold_left (fun bodylines field ->
            let name = String.capitalize_ascii field.name in
            bodylines |>
            match field.uniont, field.typ with
            | Some n, Group (Struct (_, fullname, _, _, _, _, _)) ->
              add_line (f "| %s x -> b |> set union_tag %d |> set (group t @@ Ptr Types.%s.t) x" name n fullname)
            | Some n, Slot (offset, "Void",_) -> 
              add_line (f "| %s -> b |> set union_tag %d" name n)
            | Some n, Slot (offset, typ, _) -> 
              add_line (f "| %s x -> b |> set union_tag %d |> set (field t %s %lul) x" name n typ offset)
          ) bodylines) |>
          unindent |> add_line "in ug f g" |> unindent

          
        end
        else 
          bodylines
        in


        (lines |> unindent |> add_line "end" |> add_line "",
        bodylines |> unindent |> add_line "end" |> add_line "")


    | Enum (n, fullname, enumerants) -> 
        let n = sanitize_name n in
        let lines = lines |> 
        add_line (f "module %s = struct" n) |> 
        indent |> 
        add_line "type t' =" |> indent |>
        (fun lines -> enumerants |> Array.fold_left (fun lines x ->
          lines |> add_line (x |> String.capitalize_ascii |> f "| %s")
        ) lines) |>
        unindent |>
        add_line "type t = t' g" |>
        add_line "let t =" |>
        indent |>
        add_line "let f = function" |>
        indent |>
        (fun lines -> enumerants |> Array.fold_left (fun (lines,n) x ->
          (lines |> add_line (x |> String.capitalize_ascii |> f "| %d -> %s" n), n+1)
        ) (lines, 0) |> fst) |>
        add_line "| n -> failwith (Printf.sprintf \"Invalid enum tag: %d\" n)" |>
        unindent |>
        add_line "in" |>
        add_line "let g = function" |>
        indent |>
        (fun lines -> enumerants |> Array.fold_left (fun (lines,n) x ->
          (lines |> add_line (x |> String.capitalize_ascii |> (fun x -> f "| %s -> %d" x n)), n+1)
        ) (lines, 0) |> fst) |>
        unindent |>
        add_line "in" |>
        add_line "Enum (f,g)" |>
        unindent |> 
        unindent |>
        add_line "end" |>
        add_line ""
        in

        lines, bodylines 

    | Const n ->
        lines, bodylines |> add_line (f "(* CONST %s *)" n)

    | Annotation n ->
        lines, bodylines |> add_line (f "(* ANNOTATION %s *)" n)

    | Interface (name, id) ->
        let lines = lines |>
        add_line (f "module %s = struct" name) |>
        indent |> 
        add_line "type t" |>
        add_line (f "let t : t ig = ig {id=0x%LxL}" id) |>
        add_line "" in


        (lines |> unindent |> add_line "end" |> add_line "",
        bodylines)

    | _  ->
        (lines, bodylines)



  let print_lines (lines, bodylines) =

    let output = Buffer.create 102400 in

    lines.lines |> List.rev |> List.iter (fun (n, x) ->
      Buffer.add_string output @@ String.make (2 * n) ' ';
      Buffer.add_string output x;
      Buffer.add_string output "\n";
    );
    bodylines.lines |> List.rev |> List.iter (fun (n, x) ->
      Buffer.add_string output @@ String.make (2 * n) ' ';
      Buffer.add_string output x;
      Buffer.add_string output "\n";
    );
    output |> Buffer.contents |> print_endline;
