module Type = struct
  open Decl type t let t: t structure = structure

  module Brand = struct
    open Decl type t let t : t structure = structure
  end

  module List = struct
    type t' = t
    let t' = t
    open Decl type t let t: t structure = structure

    let elementType = t |> field 0 t'
  end

  module Enum = struct
    open Decl type t let t: t structure = structure
  end

  module Interface = struct
    open Decl type t let t: t structure = structure

    let typeId = t |> field 64 uint64
    let brand = t |> field 0 Brand.t
  end

  module Struct = struct
    open Decl type t let t: t structure = structure

    let typeId = t |> field 64 uint64
    let brand = t |> field 0 Brand.t
  end

  module AnyPointer = struct
    open Decl type t let t: t structure = structure
  end


  type union =
    | Void
    | Bool
    | Int8
    | Int16
    | Int32
    | Int64
    | Uint8
    | Uint16
    | Uint32
    | Uint64
    | Float32
    | Float64
    | Text
    | Data
    | List of List.t struct_data cursor
    | Enum of Enum.t struct_data cursor
    | Struct of Struct.t struct_data cursor
    | Interface of Interface.t struct_data cursor
    | AnyPointer of AnyPointer.t struct_data cursor

  let get_union c = 
    let union_tag = t |> field 0 uint16 in
    let list_ = t |> group List.t in
    let enum_ = t |> group Enum.t in
    let struct_ = t |> group Struct.t in
    let interface_ = t |> group Interface.t in
    let anypointer_ = t |> group AnyPointer.t in
    c |> get union_tag |> result |> function
      |  0 -> Void
      |  1 -> Bool
      |  2 -> Int8
      |  3 -> Int16
      |  4 -> Int32
      |  5 -> Int64
      |  6 -> Uint8
      |  7 -> Uint16
      |  8 -> Uint32
      |  9 -> Uint64
      | 10 -> Float32
      | 11 -> Float64
      | 12 -> Text
      | 13 -> Data
      | 14 -> List (c |> getGroup list_)
      | 15 -> Enum (c |> getGroup enum_)
      | 16 -> Struct (c |> getGroup struct_)
      | 17 -> Interface (c |> getGroup interface_) 
      | 18 -> AnyPointer (c |> getGroup anypointer_) 
      | _ -> failwith "Not matched"


end

module Brand = struct
  open Decl
  include Type.Brand

  module Binding = struct
    type t let t : t structure = structure

    let union_tag = t |> field 0 uint16
    let type_ = t |> field 1 Type.t
  end


  module Scope = struct
    type t let t : t structure = structure

    let scopeId = t |> field 0 uint64
    let union_tag = t |> field 16 uint16
    let bind = t |> field 0 (Binding.t)
  end

  let scopes = t |> field 0 @@ list Scope.t
end

module Value = struct
  open Decl

  type t let t : t structure = structure

  let tag = t |> field 0 uint16

end

module Annotation = struct
  open Decl type t let t : t structure = structure

  let id = t |> field 0 uint64
  let brand = t |> field 1 Brand.t
  let value = t |> field 0 Value.t
end

module Enumerant = struct
  open Decl type t let t : t structure = structure

  let name = t |> field 0 text
  let codeOrder = t |> field 0 uint16
  let annotations = t |> field 1 Annotation.t
end

module Field = struct
  open Decl type t let t : t structure = structure

  let name = t |> field 0 text
  let codeOrder = t |> field 0 int16
  let annotations = t |> field 1 @@ list Annotation.t
  let discriminantValue = t |> field 16 uint16
  let noDiscriminant = 65535

  module Slot = struct
    open Decl type t let t : t structure = structure
    let offset = t |> field 32 uint32
    let type_ = t |> field 2 Type.t
    let defaultValue = t |> field 3 Value.t
  end

  module Group = struct
    open Decl type t let t : t structure = structure

    let typeId = t |> field 128 uint64
  end

  type union = 
    | Slot of Slot.t struct_data cursor
    | Group of Group.t struct_data cursor

  let get_union c = 
    let union_tag = t |> field 64 uint16 in
    let slot_ = t |> group Slot.t in
    let group_ = t |> group Group.t in
    match (c |> get union_tag |> result) with
    | 0 -> Slot (c |> getGroup slot_)
    | 1 -> Group (c |> getGroup group_)
    | _ -> failwith "Invalid tag value"


  (*
    union {  # tag bits [64, 80)
    slot :group {  # union tag = 0
      offset @4 :UInt32;  # bits[32, 64)
      type @5 :Type;  # ptr[2]
      defaultValue @6 :Value;  # ptr[3]
      hadExplicitDefault @10 :Bool;  # bits[128, 129)
    }
    group :group {  # union tag = 1
      typeId @7 :UInt64;  # bits[128, 192)
    }
  }
  *)
 

end

module Node = struct
  open Decl

  type t let t : t structure = structure


  module SourceInfo = struct 
    type t let t : t structure = structure

    let id = t |> field 0 uint64
    let docComment = t |> field 0 uint64

    module Member = struct
      type t let t : t structure = structure

      let docComment = t |> field 0 text
    end

    let members  = t |> field 1 @@ list Member.t
  end

  module Parameter = struct
    type t let t : t structure = structure

    let name = t |> field 0 text
    let id = t |> field 0 uint64
  end

  module NestedNode = struct
    type t let t : t structure = structure

    let name = t |> field 0 text
    let id = t |> field 0 int64
  end


  let id = t |> field 0 uint64
  let displayName = t |> field 0 text
  let displayNamePrefixLength = t |> field 64 uint32
  let scopeId = t |> field 128 uint64 
  let parameters = t |> field 5 @@ list Parameter.t
  let isGeneric = t |> field 288 bool
  let nestedNodes = t |> field 1 @@ list NestedNode.t
  let annotations = t |> field 2 Annotation.t

  module Struct = struct
    type t let t : t structure = structure

    let fields = t |> field 3 @@ list Field.t
    let discriminantCount = t |> field 240 uint16
    let discriminantOffset = t |> field 256 uint32
  end

  module Enum = struct
    type t let t : t structure = structure

    let enumerants = t |> field 3 @@ list Enumerant.t
  end

  module Interface = struct
    type t let t : t structure = structure
  end

  module Const = struct
    type t let t : t structure = structure
  end

  module Annotation = struct
    type t let t : t structure = structure

    let id = t |> field 0 int16
  end

  type union = 
    | File
    | Struct of Struct.t struct_data cursor
    | Enum of Enum.t struct_data cursor
    | Interface of Interface.t struct_data cursor
    | Const of Const.t struct_data cursor
    | Annotation of Annotation.t struct_data cursor

  let get_union c = 
    let union_tag = t |> field 96 uint16 in
    let struct_ = t |> group Struct.t in
    let enum_ = t |> group Enum.t in
    let interface_ = t |> group Interface.t in
    let const_ = t |> group Const.t in
    let annotation_ = t |> group Annotation.t in
    c |> get union_tag |> result |> function
      | 0 -> File
      | 1 -> Struct (c |> getGroup struct_)
      | 2 -> Enum (c |> getGroup enum_)
      | 3 -> Interface (c |> getGroup interface_)
      | 4 -> Const (c |> getGroup const_)
      | 5 -> Annotation (c |> getGroup annotation_)
      | _ -> failwith "unmatched"



  let enumerants = t |> field 3 Enumerant.t
end

module CapnpVersion = struct
  open Decl

  type t let t : t structure = Decl.structure

  let major = t |> field 0 uint16
  let minor = t |> field 16 uint8
  let micro = t |> field 24 uint8
end

module CodeGeneratorRequest = struct
  open Decl
  type t let t : t structure = Decl.structure

  module RequestedFile = struct
    type t let t : t structure = Decl.structure

    module Import = struct
      type t
      let t : t structure = Decl.structure

      let id = t |> field 0 Decl.uint64
      let name = t |> field 0 Decl.text
    end

    let id = t |> field 0 uint64
    let filename = t |> field 1 Decl.text
    let imports = t |> field 1 Import.t
  end

  let nodes = t |> field 0 @@ list Node.t
  let requestedFiles = t |> field 1 @@ list RequestedFile.t
  let capnpVersion = t |> field 2 CapnpVersion.t
  let sourceInfo = t |> field 3 @@ list Node.SourceInfo.t
end



