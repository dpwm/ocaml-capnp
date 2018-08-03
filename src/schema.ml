

(* This file was generated by capnptk. It is probably not a good idea to edit
   it. *) 
open Capnptk.Declarative

module Self = struct
  module Node = struct
    type t' type t = t' sgu let t : t g = sg 5 6
    module Parameter = struct
      type t' type t = t' sgu let t : t g = sg 0 1
    end
    module NestedNode = struct
      type t' type t = t' sgu let t : t g = sg 1 1
    end
    module SourceInfo = struct
      type t' type t = t' sgu let t : t g = sg 1 2
      module Member = struct
        type t' type t = t' sgu let t : t g = sg 0 1 end
    end
    module Struct = struct
      type t' type t = t' sgu let t : t g = sg 5 6 end
    module Enum = struct
      type t' type t = t' sgu let t : t g = sg 5 6 end
    module Interface = struct
      type t' type t = t' sgu let t : t g = sg 5 6
    end
    module Const = struct
      type t' type t = t' sgu let t : t g = sg 5 6 end
    module Annotation = struct
      type t' type t = t' sgu let t : t g = sg 5 6
    end
  end
  module Field = struct
    type t' type t = t' sgu let t : t g = sg 3 4
    module Slot = struct
      type t' type t = t' sgu let t : t g = sg 3 4 end
    module Group = struct
      type t' type t = t' sgu let t : t g = sg 3 4 end
    module Ordinal = struct
      type t' type t = t' sgu let t : t g = sg 3 4 end
  end
  module Enumerant = struct
    type t' type t = t' sgu let t : t g = sg 1 2 end
  module Superclass = struct
    type t' type t = t' sgu let t : t g = sg 1 1 end
  module Method = struct
    type t' type t = t' sgu let t : t g = sg 3 5 end
  module Type = struct
    type t' type t = t' sgu let t : t g = sg 3 1
    module List = struct
      type t' type t = t' sgu let t : t g = sg 3 1 end
    module Enum = struct
      type t' type t = t' sgu let t : t g = sg 3 1 end
    module Struct = struct
      type t' type t = t' sgu let t : t g = sg 3 1 end
    module Interface = struct
      type t' type t = t' sgu let t : t g = sg 3 1
    end
    module AnyPointer = struct
      type t' type t = t' sgu let t : t g = sg 3 1
      module Unconstrained = struct
        type t' type t = t' sgu let t : t g = sg 3 1
      end
      module Parameter = struct
        type t' type t = t' sgu let t : t g = sg 3 1
      end
      module ImplicitMethodParameter = struct
        type t' type t = t' sgu let t : t g = sg 3 1
      end
    end
  end
  module Brand = struct
    type t' type t = t' sgu let t : t g = sg 0 1
    module Scope = struct
      type t' type t = t' sgu let t : t g = sg 2 1 end
    module Binding = struct
      type t' type t = t' sgu let t : t g = sg 1 1 end
  end
  module Value = struct
    type t' type t = t' sgu let t : t g = sg 2 1 end
  module Annotation = struct
    type t' type t = t' sgu let t : t g = sg 1 2 end
  module ElementSize = struct
    type t = | Empty | Bit | Byte | TwoBytes | FourBytes | EightBytes
    | Pointer | InlineComposite let t = let f = function | 0 -> Empty
    | 1 -> Bit | 2 -> Byte | 3 -> TwoBytes | 4 -> FourBytes | 5 -> EightBytes
    | 6 -> Pointer | 7 -> InlineComposite | n -> raise
    (Capnptk.OrdinalError n) in let g = function | Empty -> 0 | Bit -> 1
    | Byte -> 2 | TwoBytes -> 3 | FourBytes -> 4 | EightBytes -> 5
    | Pointer -> 6 | InlineComposite -> 7 in Enum(f, g)
  end
  module CapnpVersion = struct
    type t' type t = t' sgu let t : t g = sg 1 0
  end
  module CodeGeneratorRequest = struct
    type t' type t = t' sgu let t : t g = sg 0 4
    module RequestedFile = struct
      type t' type t = t' sgu let t : t g = sg 1 2
      module Import = struct
        type t' type t = t' sgu let t : t g = sg 1 1 end
    end
  end
end

(* Now we begin the body. This is what will be referenced by outside scripts,
   so the first thing we do in each module is alias to the types declared
   above.  *) 



module Node = struct
  include (Self.Node : Type with type t = Self.Node.t)
  
  module Parameter = struct
    include (Self.Node.Parameter : Type with type t = Self.Node.Parameter.t)
    let name = field t Text 0l
  end
  
  module NestedNode = struct
    include (Self.Node.NestedNode : Type with type t = Self.Node.NestedNode.t)
    let name = field t Text 0l
    let id = field t UInt64 0l
  end
  
  module SourceInfo = struct
    include (Self.Node.SourceInfo : Type with type t = Self.Node.SourceInfo.t)
    
    module Member = struct
      include (Self.Node.SourceInfo.Member : Type with type t = Self.Node.SourceInfo.Member.t)
      let docComment = field t Text 0l
    end
    let id = field t UInt64 0l
    let docComment = field t Text 0l
    let members = field t (List Self.Node.SourceInfo.Member.t) 1l
  end
  
  module Struct = struct
    include (Self.Node.Struct : Type with type t = Self.Node.Struct.t)
    let dataWordCount = field t UInt16 112l
    let pointerCount = field t UInt16 192l
    let preferredListEncoding = field t Self.ElementSize.t 208l
    let isGroup = field t Bool 224l
    let discriminantCount = field t UInt16 240l
    let discriminantOffset = field t UInt32 256l
    let fields = field t (List Self.Field.t) 3l
  end
  
  module Enum = struct
    include (Self.Node.Enum : Type with type t = Self.Node.Enum.t)
    let enumerants = field t (List Self.Enumerant.t) 3l
  end
  
  module Interface = struct
    include (Self.Node.Interface : Type with type t = Self.Node.Interface.t)
    let methods = field t (List Self.Method.t) 3l
    let superclasses = field t (List Self.Superclass.t) 4l
  end
  
  module Const = struct
    include (Self.Node.Const : Type with type t = Self.Node.Const.t)
    let type_ = field t Self.Type.t 3l
    let value = field t Self.Value.t 4l
  end
  
  module Annotation = struct
    include (Self.Node.Annotation : Type with type t = Self.Node.Annotation.t)
    let type_ = field t Self.Type.t 3l
    let targetsFile = field t Bool 112l
    let targetsConst = field t Bool 113l
    let targetsEnum = field t Bool 114l
    let targetsEnumerant = field t Bool 115l
    let targetsStruct = field t Bool 116l
    let targetsField = field t Bool 117l
    let targetsUnion = field t Bool 118l
    let targetsGroup = field t Bool 119l
    let targetsInterface = field t Bool 120l
    let targetsMethod = field t Bool 121l
    let targetsParam = field t Bool 122l
    let targetsAnnotation = field t Bool 123l
  end
  
  type union =
    | File
    | Struct of Self.Node.Struct.t
    | Enum of Self.Node.Enum.t
    | Interface of Self.Node.Interface.t
    | Const of Self.Node.Const.t
    | Annotation of Self.Node.Annotation.t
  let union =
    let union_tag = field t UInt16 96l in
    let f c =
      match c => union_tag with
        | 0 -> File
        | 1 -> Struct (c => (group t (Self.Node.Struct.t)))
        | 2 -> Enum (c => (group t (Self.Node.Enum.t)))
        | 3 -> Interface (c => (group t (Self.Node.Interface.t)))
        | 4 -> Const (c => (group t (Self.Node.Const.t)))
        | 5 -> Annotation (c => (group t (Self.Node.Annotation.t)))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | File -> b |> set union_tag 0;
      | Struct v -> b |> set (group t (Self.Node.Struct.t)) v |> set union_tag 1
      | Enum v -> b |> set (group t (Self.Node.Enum.t)) v |> set union_tag 2
      | Interface v -> b |> set (group t (Self.Node.Interface.t)) v |> set union_tag 3
      | Const v -> b |> set (group t (Self.Node.Const.t)) v |> set union_tag 4
      | Annotation v -> b |> set (group t (Self.Node.Annotation.t)) v |> set union_tag 5
    in ug f g
  let id = field t UInt64 0l
  let displayName = field t Text 0l
  let displayNamePrefixLength = field t UInt32 64l
  let scopeId = field t UInt64 128l
  let nestedNodes = field t (List Self.Node.NestedNode.t) 1l
  let annotations = field t (List Self.Annotation.t) 2l
  let parameters = field t (List Self.Node.Parameter.t) 5l
  let isGeneric = field t Bool 288l
end

module Field = struct
  include (Self.Field : Type with type t = Self.Field.t)
  let noDiscriminant = 65535
  
  module Slot = struct
    include (Self.Field.Slot : Type with type t = Self.Field.Slot.t)
    let offset = field t UInt32 32l
    let type_ = field t Self.Type.t 2l
    let defaultValue = field t Self.Value.t 3l
    let hadExplicitDefault = field t Bool 128l
  end
  
  module Group = struct
    include (Self.Field.Group : Type with type t = Self.Field.Group.t)
    let typeId = field t UInt64 128l
  end
  
  module Ordinal = struct
    include (Self.Field.Ordinal : Type with type t = Self.Field.Ordinal.t)
    
    type union =
      | Implicit
      | Explicit of int
    let union =
      let union_tag = field t UInt16 80l in
      let f c =
        match c => union_tag with
          | 0 -> Implicit
          | 1 -> Explicit (c => (field t UInt16 96l))
          | n -> raise (Capnptk.OrdinalError n)
      in
      let g b = function
        | Implicit -> b |> set union_tag 0;
        | Explicit v -> b |> set (field t UInt16 96l) v |> set union_tag 1
      in ug f g
  end
  
  type union =
    | Slot of Self.Field.Slot.t
    | Group of Self.Field.Group.t
  let union =
    let union_tag = field t UInt16 64l in
    let f c =
      match c => union_tag with
        | 0 -> Slot (c => (group t (Self.Field.Slot.t)))
        | 1 -> Group (c => (group t (Self.Field.Group.t)))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | Slot v -> b |> set (group t (Self.Field.Slot.t)) v |> set union_tag 0
      | Group v -> b |> set (group t (Self.Field.Group.t)) v |> set union_tag 1
    in ug f g
  let name = field t Text 0l
  let codeOrder = field t UInt16 0l
  let annotations = field t (List Self.Annotation.t) 1l
  let discriminantValue = field t UInt16 ~default:(65535) 16l
  let ordinal = group t (Self.Field.Ordinal.t)
end

module Enumerant = struct
  include (Self.Enumerant : Type with type t = Self.Enumerant.t)
  let name = field t Text 0l
  let codeOrder = field t UInt16 0l
  let annotations = field t (List Self.Annotation.t) 1l
end

module Superclass = struct
  include (Self.Superclass : Type with type t = Self.Superclass.t)
  let id = field t UInt64 0l
  let brand = field t Self.Brand.t 0l
end

module Method = struct
  include (Self.Method : Type with type t = Self.Method.t)
  let name = field t Text 0l
  let codeOrder = field t UInt16 0l
  let paramStructType = field t UInt64 64l
  let resultStructType = field t UInt64 128l
  let annotations = field t (List Self.Annotation.t) 1l
  let paramBrand = field t Self.Brand.t 2l
  let resultBrand = field t Self.Brand.t 3l
  let implicitParameters = field t (List Self.Node.Parameter.t) 4l
end

module Type = struct
  include (Self.Type : Type with type t = Self.Type.t)
  
  module List = struct
    include (Self.Type.List : Type with type t = Self.Type.List.t)
    let elementType = field t Self.Type.t 0l
  end
  
  module Enum = struct
    include (Self.Type.Enum : Type with type t = Self.Type.Enum.t)
    let typeId = field t UInt64 64l
    let brand = field t Self.Brand.t 0l
  end
  
  module Struct = struct
    include (Self.Type.Struct : Type with type t = Self.Type.Struct.t)
    let typeId = field t UInt64 64l
    let brand = field t Self.Brand.t 0l
  end
  
  module Interface = struct
    include (Self.Type.Interface : Type with type t = Self.Type.Interface.t)
    let typeId = field t UInt64 64l
    let brand = field t Self.Brand.t 0l
  end
  
  module AnyPointer = struct
    include (Self.Type.AnyPointer : Type with type t = Self.Type.AnyPointer.t)
    
    module Unconstrained = struct
      include (Self.Type.AnyPointer.Unconstrained : Type with type t = Self.Type.AnyPointer.Unconstrained.t)
      
      type union =
        | AnyKind
        | Struct
        | List
        | Capability
      let union =
        let union_tag = field t UInt16 80l in
        let f c =
          match c => union_tag with
            | 0 -> AnyKind
            | 1 -> Struct
            | 2 -> List
            | 3 -> Capability
            | n -> raise (Capnptk.OrdinalError n)
        in
        let g b = function
          | AnyKind -> b |> set union_tag 0;
          | Struct -> b |> set union_tag 1;
          | List -> b |> set union_tag 2;
          | Capability -> b |> set union_tag 3;
        in ug f g
    end
    
    module Parameter = struct
      include (Self.Type.AnyPointer.Parameter : Type with type t = Self.Type.AnyPointer.Parameter.t)
      let scopeId = field t UInt64 128l
      let parameterIndex = field t UInt16 80l
    end
    
    module ImplicitMethodParameter = struct
      include (Self.Type.AnyPointer.ImplicitMethodParameter : Type with type t = Self.Type.AnyPointer.ImplicitMethodParameter.t)
      let parameterIndex = field t UInt16 80l
    end
    
    type union =
      | Unconstrained of Self.Type.AnyPointer.Unconstrained.t
      | Parameter of Self.Type.AnyPointer.Parameter.t
      | ImplicitMethodParameter of Self.Type.AnyPointer.ImplicitMethodParameter.t
    let union =
      let union_tag = field t UInt16 64l in
      let f c =
        match c => union_tag with
          | 0 -> Unconstrained (c => (group t (Self.Type.AnyPointer.Unconstrained.t)))
          | 1 -> Parameter (c => (group t (Self.Type.AnyPointer.Parameter.t)))
          | 2 -> ImplicitMethodParameter (c => (group t (Self.Type.AnyPointer.ImplicitMethodParameter.t)))
          | n -> raise (Capnptk.OrdinalError n)
      in
      let g b = function
        | Unconstrained v -> b |> set (group t (Self.Type.AnyPointer.Unconstrained.t)) v |> set union_tag 0
        | Parameter v -> b |> set (group t (Self.Type.AnyPointer.Parameter.t)) v |> set union_tag 1
        | ImplicitMethodParameter v -> b |> set (group t (Self.Type.AnyPointer.ImplicitMethodParameter.t)) v |> set union_tag 2
      in ug f g
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
    | List of Self.Type.List.t
    | Enum of Self.Type.Enum.t
    | Struct of Self.Type.Struct.t
    | Interface of Self.Type.Interface.t
    | AnyPointer of Self.Type.AnyPointer.t
  let union =
    let union_tag = field t UInt16 0l in
    let f c =
      match c => union_tag with
        | 0 -> Void
        | 1 -> Bool
        | 2 -> Int8
        | 3 -> Int16
        | 4 -> Int32
        | 5 -> Int64
        | 6 -> Uint8
        | 7 -> Uint16
        | 8 -> Uint32
        | 9 -> Uint64
        | 10 -> Float32
        | 11 -> Float64
        | 12 -> Text
        | 13 -> Data
        | 14 -> List (c => (group t (Self.Type.List.t)))
        | 15 -> Enum (c => (group t (Self.Type.Enum.t)))
        | 16 -> Struct (c => (group t (Self.Type.Struct.t)))
        | 17 -> Interface (c => (group t (Self.Type.Interface.t)))
        | 18 -> AnyPointer (c => (group t (Self.Type.AnyPointer.t)))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | Void -> b |> set union_tag 0;
      | Bool -> b |> set union_tag 1;
      | Int8 -> b |> set union_tag 2;
      | Int16 -> b |> set union_tag 3;
      | Int32 -> b |> set union_tag 4;
      | Int64 -> b |> set union_tag 5;
      | Uint8 -> b |> set union_tag 6;
      | Uint16 -> b |> set union_tag 7;
      | Uint32 -> b |> set union_tag 8;
      | Uint64 -> b |> set union_tag 9;
      | Float32 -> b |> set union_tag 10;
      | Float64 -> b |> set union_tag 11;
      | Text -> b |> set union_tag 12;
      | Data -> b |> set union_tag 13;
      | List v -> b |> set (group t (Self.Type.List.t)) v |> set union_tag 14
      | Enum v -> b |> set (group t (Self.Type.Enum.t)) v |> set union_tag 15
      | Struct v -> b |> set (group t (Self.Type.Struct.t)) v |> set union_tag 16
      | Interface v -> b |> set (group t (Self.Type.Interface.t)) v |> set union_tag 17
      | AnyPointer v -> b |> set (group t (Self.Type.AnyPointer.t)) v |> set union_tag 18
    in ug f g
end

module Brand = struct
  include (Self.Brand : Type with type t = Self.Brand.t)
  
  module Scope = struct
    include (Self.Brand.Scope : Type with type t = Self.Brand.Scope.t)
    
    type union =
      | Bind of Self.Brand.Binding.t array
      | Inherit
    let union =
      let union_tag = field t UInt16 64l in
      let f c =
        match c => union_tag with
          | 0 -> Bind (c => (field t (List Self.Brand.Binding.t) 0l))
          | 1 -> Inherit
          | n -> raise (Capnptk.OrdinalError n)
      in
      let g b = function
        | Bind v -> b |> set (field t (List Self.Brand.Binding.t) 0l) v |> set union_tag 0
        | Inherit -> b |> set union_tag 1;
      in ug f g
    let scopeId = field t UInt64 0l
  end
  
  module Binding = struct
    include (Self.Brand.Binding : Type with type t = Self.Brand.Binding.t)
    
    type union =
      | Unbound
      | Type of Self.Type.t
    let union =
      let union_tag = field t UInt16 0l in
      let f c =
        match c => union_tag with
          | 0 -> Unbound
          | 1 -> Type (c => (field t Self.Type.t 0l))
          | n -> raise (Capnptk.OrdinalError n)
      in
      let g b = function
        | Unbound -> b |> set union_tag 0;
        | Type v -> b |> set (field t Self.Type.t 0l) v |> set union_tag 1
      in ug f g
  end
  let scopes = field t (List Self.Brand.Scope.t) 0l
end

module Value = struct
  include (Self.Value : Type with type t = Self.Value.t)
  
  type union =
    | Void
    | Bool of bool
    | Int8 of int
    | Int16 of int
    | Int32 of int32
    | Int64 of int64
    | Uint8 of int
    | Uint16 of int
    | Uint32 of int32
    | Uint64 of int64
    | Float32 of float
    | Float64 of float
    | Text of string
    | Data of string
    | List of unit c
    | Enum of int
    | Struct of unit c
    | Interface
    | AnyPointer of unit c
  let union =
    let union_tag = field t UInt16 0l in
    let f c =
      match c => union_tag with
        | 0 -> Void
        | 1 -> Bool (c => (field t Bool 16l))
        | 2 -> Int8 (c => (field t Int8 16l))
        | 3 -> Int16 (c => (field t Int16 16l))
        | 4 -> Int32 (c => (field t Int32 32l))
        | 5 -> Int64 (c => (field t Int64 64l))
        | 6 -> Uint8 (c => (field t UInt8 16l))
        | 7 -> Uint16 (c => (field t UInt16 16l))
        | 8 -> Uint32 (c => (field t UInt32 32l))
        | 9 -> Uint64 (c => (field t UInt64 64l))
        | 10 -> Float32 (c => (field t Float32 32l))
        | 11 -> Float64 (c => (field t Float64 64l))
        | 12 -> Text (c => (field t Text 0l))
        | 13 -> Data (c => (field t Data 0l))
        | 14 -> List (c => (field t (Ptr Void) 0l))
        | 15 -> Enum (c => (field t UInt16 16l))
        | 16 -> Struct (c => (field t (Ptr Void) 0l))
        | 17 -> Interface
        | 18 -> AnyPointer (c => (field t (Ptr Void) 0l))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | Void -> b |> set union_tag 0;
      | Bool v -> b |> set (field t Bool 16l) v |> set union_tag 1
      | Int8 v -> b |> set (field t Int8 16l) v |> set union_tag 2
      | Int16 v -> b |> set (field t Int16 16l) v |> set union_tag 3
      | Int32 v -> b |> set (field t Int32 32l) v |> set union_tag 4
      | Int64 v -> b |> set (field t Int64 64l) v |> set union_tag 5
      | Uint8 v -> b |> set (field t UInt8 16l) v |> set union_tag 6
      | Uint16 v -> b |> set (field t UInt16 16l) v |> set union_tag 7
      | Uint32 v -> b |> set (field t UInt32 32l) v |> set union_tag 8
      | Uint64 v -> b |> set (field t UInt64 64l) v |> set union_tag 9
      | Float32 v -> b |> set (field t Float32 32l) v |> set union_tag 10
      | Float64 v -> b |> set (field t Float64 64l) v |> set union_tag 11
      | Text v -> b |> set (field t Text 0l) v |> set union_tag 12
      | Data v -> b |> set (field t Data 0l) v |> set union_tag 13
      | List v -> b |> set (field t (Ptr Void) 0l) v |> set union_tag 14
      | Enum v -> b |> set (field t UInt16 16l) v |> set union_tag 15
      | Struct v -> b |> set (field t (Ptr Void) 0l) v |> set union_tag 16
      | Interface -> b |> set union_tag 17;
      | AnyPointer v -> b |> set (field t (Ptr Void) 0l) v |> set union_tag 18
    in ug f g
end

module Annotation = struct
  include (Self.Annotation : Type with type t = Self.Annotation.t)
  let id = field t UInt64 0l
  let value = field t Self.Value.t 0l
  let brand = field t Self.Brand.t 1l
end

module ElementSize = struct
  include (Self.ElementSize : Type with type t = Self.ElementSize.t)
end

module CapnpVersion = struct
  include (Self.CapnpVersion : Type with type t = Self.CapnpVersion.t)
  let major = field t UInt16 0l
  let minor = field t UInt8 16l
  let micro = field t UInt8 24l
end

module CodeGeneratorRequest = struct
  include (Self.CodeGeneratorRequest : Type with type t = Self.CodeGeneratorRequest.t)
  
  module RequestedFile = struct
    include (Self.CodeGeneratorRequest.RequestedFile : Type with type t = Self.CodeGeneratorRequest.RequestedFile.t)
    
    module Import = struct
      include (Self.CodeGeneratorRequest.RequestedFile.Import : Type with type t = Self.CodeGeneratorRequest.RequestedFile.Import.t)
      let id = field t UInt64 0l
      let name = field t Text 0l
    end
    let id = field t UInt64 0l
    let filename = field t Text 0l
    let imports = field t (List Self.CodeGeneratorRequest.RequestedFile.Import.t) 1l
  end
  let nodes = field t (List Self.Node.t) 0l
  let requestedFiles = field t (List Self.CodeGeneratorRequest.RequestedFile.t) 1l
  let capnpVersion = field t Self.CapnpVersion.t 2l
  let sourceInfo = field t (List Self.Node.SourceInfo.t) 3l
end

