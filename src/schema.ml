(* schema.capnp *)

(* This file was auto-generated from a capnproto schema.
   You should not need to edit this file!                *)

(* The Types module pre-defines all the types used. 
   This simplifies implementation by acting as forward definitions.  *)

module Types = struct
  
  open Capnptk.Declarative
  
  module Node = struct
    type t
    let t : t sg = sg 5 6
    
    module Parameter = struct
      type t
      let t : t sg = sg 0 1
    end
    
    module NestedNode = struct
      type t
      let t : t sg = sg 1 1
    end
    
    module SourceInfo = struct
      type t
      let t : t sg = sg 1 2
      
      module Member = struct
        type t
        let t : t sg = sg 0 1
      end
    end
    
    module Struct = struct
      type t
      let t : t sg = sg 5 6
    end
    
    module Enum = struct
      type t
      let t : t sg = sg 5 6
    end
    
    module Interface = struct
      type t
      let t : t sg = sg 5 6
    end
    
    module Const = struct
      type t
      let t : t sg = sg 5 6
    end
    
    module Annotation = struct
      type t
      let t : t sg = sg 5 6
    end
  end
  
  module Field = struct
    type t
    let t : t sg = sg 3 4
    
    module Slot = struct
      type t
      let t : t sg = sg 3 4
    end
    
    module Group = struct
      type t
      let t : t sg = sg 3 4
    end
    
    module Ordinal = struct
      type t
      let t : t sg = sg 3 4
    end
  end
  
  module Enumerant = struct
    type t
    let t : t sg = sg 1 2
  end
  
  module Superclass = struct
    type t
    let t : t sg = sg 1 1
  end
  
  module Method = struct
    type t
    let t : t sg = sg 3 5
  end
  
  module Type = struct
    type t
    let t : t sg = sg 3 1
    
    module List = struct
      type t
      let t : t sg = sg 3 1
    end
    
    module Enum = struct
      type t
      let t : t sg = sg 3 1
    end
    
    module Struct = struct
      type t
      let t : t sg = sg 3 1
    end
    
    module Interface = struct
      type t
      let t : t sg = sg 3 1
    end
    
    module AnyPointer = struct
      type t
      let t : t sg = sg 3 1
      
      module Unconstrained = struct
        type t
        let t : t sg = sg 3 1
      end
      
      module Parameter = struct
        type t
        let t : t sg = sg 3 1
      end
      
      module ImplicitMethodParameter = struct
        type t
        let t : t sg = sg 3 1
      end
    end
  end
  
  module Brand = struct
    type t
    let t : t sg = sg 0 1
    
    module Scope = struct
      type t
      let t : t sg = sg 2 1
    end
    
    module Binding = struct
      type t
      let t : t sg = sg 1 1
    end
  end
  
  module Value = struct
    type t
    let t : t sg = sg 2 1
  end
  
  module Annotation = struct
    type t
    let t : t sg = sg 1 2
  end
  
  module ElementSize = struct
    type t' =
      | Empty
      | Bit
      | Byte
      | TwoBytes
      | FourBytes
      | EightBytes
      | Pointer
      | InlineComposite
    type t = t' g
    let t =
      let f = function
        | 0 -> Empty
        | 1 -> Bit
        | 2 -> Byte
        | 3 -> TwoBytes
        | 4 -> FourBytes
        | 5 -> EightBytes
        | 6 -> Pointer
        | 7 -> InlineComposite
        | n -> failwith (Printf.sprintf "Invalid enum tag: %d" n)
      in
      let g = function
        | Empty -> 0
        | Bit -> 1
        | Byte -> 2
        | TwoBytes -> 3
        | FourBytes -> 4
        | EightBytes -> 5
        | Pointer -> 6
        | InlineComposite -> 7
      in
      Enum (f,g)
  end
  
  module CapnpVersion = struct
    type t
    let t : t sg = sg 1 0
  end
  
  module CodeGeneratorRequest = struct
    type t
    let t : t sg = sg 0 4
    
    module RequestedFile = struct
      type t
      let t : t sg = sg 1 2
      
      module Import = struct
        type t
        let t : t sg = sg 1 1
      end
    end
  end
end

(* The rest of the file contains the real declarations. *)
module Decls = struct
  open Capnptk.Declarative
  
  module Node = struct
    type t = Types.Node.t
    let t = Types.Node.t
    module Parameter = struct
      type t = Types.Node.Parameter.t
      let t = Types.Node.Parameter.t
      
      (* Field definitions *)
      let name = field t Text 0l
    end
    
    module NestedNode = struct
      type t = Types.Node.NestedNode.t
      let t = Types.Node.NestedNode.t
      
      (* Field definitions *)
      let name = field t Text 0l
      let id = field t UInt64 0l
    end
    
    module SourceInfo = struct
      type t = Types.Node.SourceInfo.t
      let t = Types.Node.SourceInfo.t
      module Member = struct
        type t = Types.Node.SourceInfo.Member.t
        let t = Types.Node.SourceInfo.Member.t
        
        (* Field definitions *)
        let docComment = field t Text 0l
      end
      
      
      (* Field definitions *)
      let id = field t UInt64 0l
      let docComment = field t Text 0l
      let members = field t (List (ptr Types.Node.SourceInfo.Member.t)) 1l
    end
    
    module Struct = struct
      type t = Types.Node.Struct.t
      let t = Types.Node.Struct.t
      
      (* Field definitions *)
      let dataWordCount = field t UInt16 112l
      let pointerCount = field t UInt16 192l
      let preferredListEncoding = field t Types.ElementSize.t 208l
      let isGroup = field t Bool 224l
      let discriminantCount = field t UInt16 240l
      let discriminantOffset = field t UInt32 256l
      let fields = field t (List (ptr Types.Field.t)) 3l
    end
    
    module Enum = struct
      type t = Types.Node.Enum.t
      let t = Types.Node.Enum.t
      
      (* Field definitions *)
      let enumerants = field t (List (ptr Types.Enumerant.t)) 3l
    end
    
    module Interface = struct
      type t = Types.Node.Interface.t
      let t = Types.Node.Interface.t
      
      (* Field definitions *)
      let methods = field t (List (ptr Types.Method.t)) 3l
      let superclasses = field t (List (ptr Types.Superclass.t)) 4l
    end
    
    module Const = struct
      type t = Types.Node.Const.t
      let t = Types.Node.Const.t
      
      (* Field definitions *)
      let type_ = field t (ptr Types.Type.t) 3l
      let value = field t (ptr Types.Value.t) 4l
    end
    
    module Annotation = struct
      type t = Types.Node.Annotation.t
      let t = Types.Node.Annotation.t
      
      (* Field definitions *)
      let type_ = field t (ptr Types.Type.t) 3l
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
    
    
    (* Field definitions *)
    let id = field t UInt64 0l
    let displayName = field t Text 0l
    let displayNamePrefixLength = field t UInt32 64l
    let scopeId = field t UInt64 128l
    let nestedNodes = field t (List (ptr Types.Node.NestedNode.t)) 1l
    let annotations = field t (List (ptr Types.Annotation.t)) 2l
    let parameters = field t (List (ptr Types.Node.Parameter.t)) 5l
    let isGeneric = field t Bool 288l
    
    (* Unnamed union *)
    
    type union =
      | File
      | Struct of Types.Node.Struct.t s c
      | Enum of Types.Node.Enum.t s c
      | Interface of Types.Node.Interface.t s c
      | Const of Types.Node.Const.t s c
      | Annotation of Types.Node.Annotation.t s c
    let union =
      let union_tag = field t UInt16 96l in
      let f c =
        match c |> get union_tag with
          | 0 -> File
          | 1 -> Struct (c |> get (group t @@ Ptr Types.Node.Struct.t))
          | 2 -> Enum (c |> get (group t @@ Ptr Types.Node.Enum.t))
          | 3 -> Interface (c |> get (group t @@ Ptr Types.Node.Interface.t))
          | 4 -> Const (c |> get (group t @@ Ptr Types.Node.Const.t))
          | 5 -> Annotation (c |> get (group t @@ Ptr Types.Node.Annotation.t))
          | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
      in let g b = function
        | File -> b |> set union_tag 0
        | Struct x -> b |> set union_tag 1 |> set (group t @@ Ptr Types.Node.Struct.t) x
        | Enum x -> b |> set union_tag 2 |> set (group t @@ Ptr Types.Node.Enum.t) x
        | Interface x -> b |> set union_tag 3 |> set (group t @@ Ptr Types.Node.Interface.t) x
        | Const x -> b |> set union_tag 4 |> set (group t @@ Ptr Types.Node.Const.t) x
        | Annotation x -> b |> set union_tag 5 |> set (group t @@ Ptr Types.Node.Annotation.t) x
      in ug f g
  end
  
  module Field = struct
    type t = Types.Field.t
    let t = Types.Field.t


    module Slot = struct
      type t = Types.Field.Slot.t
      let t = Types.Field.Slot.t
      
      (* Field definitions *)
      let offset = field t UInt32 32l
      let type_ = field t (ptr Types.Type.t) 2l
      let defaultValue = field t (ptr Types.Value.t) 3l
      let hadExplicitDefault = field t Bool 128l
    end
    
    module Group = struct
      type t = Types.Field.Group.t
      let t = Types.Field.Group.t
      
      (* Field definitions *)
      let typeId = field t UInt64 128l
    end
    
    module Ordinal = struct
      type t = Types.Field.Ordinal.t
      let t = Types.Field.Ordinal.t
      
      (* Field definitions *)
      
      (* Unnamed union *)
      
      type union =
        | Implicit
        | Explicit of int
      let union =
        let union_tag = field t UInt16 80l in
        let f c =
          match c |> get union_tag with
            | 0 -> Implicit
            | 1 -> Explicit (get (field t UInt16 96l) c)
            | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
        in let g b = function
          | Implicit -> b |> set union_tag 0
          | Explicit x -> b |> set union_tag 1 |> set (field t UInt16 96l) x
        in ug f g
    end
    
    
    (* Field definitions *)
    let name = field t Text 0l
    let codeOrder = field t UInt16 0l
    let annotations = field t (List (ptr Types.Annotation.t)) 1l
    let noDiscriminant = 0xffff
    let discriminantValue = field ~default:noDiscriminant t UInt16 16l
    let ordinal = group t (ptr Types.Field.Ordinal.t)
    
    (* Unnamed union *)
    
    type union =
      | Slot of Types.Field.Slot.t s c
      | Group of Types.Field.Group.t s c
    let union =
      let union_tag = field t UInt16 64l in
      let f c =
        match c |> get union_tag with
          | 0 -> Slot (c |> get (group t @@ Ptr Types.Field.Slot.t))
          | 1 -> Group (c |> get (group t @@ Ptr Types.Field.Group.t))
          | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
      in let g b = function
        | Slot x -> b |> set union_tag 0 |> set (group t @@ Ptr Types.Field.Slot.t) x
        | Group x -> b |> set union_tag 1 |> set (group t @@ Ptr Types.Field.Group.t) x
      in ug f g
  end
  
  module Enumerant = struct
    type t = Types.Enumerant.t
    let t = Types.Enumerant.t
    
    (* Field definitions *)
    let name = field t Text 0l
    let codeOrder = field t UInt16 0l
    let annotations = field t (List (ptr Types.Annotation.t)) 1l
  end
  
  module Superclass = struct
    type t = Types.Superclass.t
    let t = Types.Superclass.t
    
    (* Field definitions *)
    let id = field t UInt64 0l
    let brand = field t (ptr Types.Brand.t) 0l
  end
  
  module Method = struct
    type t = Types.Method.t
    let t = Types.Method.t
    
    (* Field definitions *)
    let name = field t Text 0l
    let codeOrder = field t UInt16 0l
    let paramStructType = field t UInt64 64l
    let resultStructType = field t UInt64 128l
    let annotations = field t (List (ptr Types.Annotation.t)) 1l
    let paramBrand = field t (ptr Types.Brand.t) 2l
    let resultBrand = field t (ptr Types.Brand.t) 3l
    let implicitParameters = field t (List (ptr Types.Node.Parameter.t)) 4l
  end
  
  module Type = struct
    type t = Types.Type.t
    let t = Types.Type.t
    module List = struct
      type t = Types.Type.List.t
      let t = Types.Type.List.t
      
      (* Field definitions *)
      let elementType = field t (ptr Types.Type.t) 0l
    end
    
    module Enum = struct
      type t = Types.Type.Enum.t
      let t = Types.Type.Enum.t
      
      (* Field definitions *)
      let typeId = field t UInt64 64l
      let brand = field t (ptr Types.Brand.t) 0l
    end
    
    module Struct = struct
      type t = Types.Type.Struct.t
      let t = Types.Type.Struct.t
      
      (* Field definitions *)
      let typeId = field t UInt64 64l
      let brand = field t (ptr Types.Brand.t) 0l
    end
    
    module Interface = struct
      type t = Types.Type.Interface.t
      let t = Types.Type.Interface.t
      
      (* Field definitions *)
      let typeId = field t UInt64 64l
      let brand = field t (ptr Types.Brand.t) 0l
    end
    
    module AnyPointer = struct
      type t = Types.Type.AnyPointer.t
      let t = Types.Type.AnyPointer.t
      module Unconstrained = struct
        type t = Types.Type.AnyPointer.Unconstrained.t
        let t = Types.Type.AnyPointer.Unconstrained.t
        
        (* Field definitions *)
        
        (* Unnamed union *)
        
        type union =
          | AnyKind
          | Struct
          | List
          | Capability
        let union =
          let union_tag = field t UInt16 80l in
          let f c =
            match c |> get union_tag with
              | 0 -> AnyKind
              | 1 -> Struct
              | 2 -> List
              | 3 -> Capability
              | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
          in let g b = function
            | AnyKind -> b |> set union_tag 0
            | Struct -> b |> set union_tag 1
            | List -> b |> set union_tag 2
            | Capability -> b |> set union_tag 3
          in ug f g
      end
      
      module Parameter = struct
        type t = Types.Type.AnyPointer.Parameter.t
        let t = Types.Type.AnyPointer.Parameter.t
        
        (* Field definitions *)
        let scopeId = field t UInt64 128l
        let parameterIndex = field t UInt16 80l
      end
      
      module ImplicitMethodParameter = struct
        type t = Types.Type.AnyPointer.ImplicitMethodParameter.t
        let t = Types.Type.AnyPointer.ImplicitMethodParameter.t
        
        (* Field definitions *)
        let parameterIndex = field t UInt16 80l
      end
      
      
      (* Field definitions *)
      
      (* Unnamed union *)
      
      type union =
        | Unconstrained of Types.Type.AnyPointer.Unconstrained.t s c
        | Parameter of Types.Type.AnyPointer.Parameter.t s c
        | ImplicitMethodParameter of Types.Type.AnyPointer.ImplicitMethodParameter.t s c
      let union =
        let union_tag = field t UInt16 64l in
        let f c =
          match c |> get union_tag with
            | 0 -> Unconstrained (c |> get (group t @@ Ptr Types.Type.AnyPointer.Unconstrained.t))
            | 1 -> Parameter (c |> get (group t @@ Ptr Types.Type.AnyPointer.Parameter.t))
            | 2 -> ImplicitMethodParameter (c |> get (group t @@ Ptr Types.Type.AnyPointer.ImplicitMethodParameter.t))
            | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
        in let g b = function
          | Unconstrained x -> b |> set union_tag 0 |> set (group t @@ Ptr Types.Type.AnyPointer.Unconstrained.t) x
          | Parameter x -> b |> set union_tag 1 |> set (group t @@ Ptr Types.Type.AnyPointer.Parameter.t) x
          | ImplicitMethodParameter x -> b |> set union_tag 2 |> set (group t @@ Ptr Types.Type.AnyPointer.ImplicitMethodParameter.t) x
        in ug f g
    end
    
    
    (* Field definitions *)
    
    (* Unnamed union *)
    
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
      | List of Types.Type.List.t s c
      | Enum of Types.Type.Enum.t s c
      | Struct of Types.Type.Struct.t s c
      | Interface of Types.Type.Interface.t s c
      | AnyPointer of Types.Type.AnyPointer.t s c
    let union =
      let union_tag = field t UInt16 0l in
      let f c =
        match c |> get union_tag with
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
          | 14 -> List (c |> get (group t @@ Ptr Types.Type.List.t))
          | 15 -> Enum (c |> get (group t @@ Ptr Types.Type.Enum.t))
          | 16 -> Struct (c |> get (group t @@ Ptr Types.Type.Struct.t))
          | 17 -> Interface (c |> get (group t @@ Ptr Types.Type.Interface.t))
          | 18 -> AnyPointer (c |> get (group t @@ Ptr Types.Type.AnyPointer.t))
          | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
      in let g b = function
        | Void -> b |> set union_tag 0
        | Bool -> b |> set union_tag 1
        | Int8 -> b |> set union_tag 2
        | Int16 -> b |> set union_tag 3
        | Int32 -> b |> set union_tag 4
        | Int64 -> b |> set union_tag 5
        | Uint8 -> b |> set union_tag 6
        | Uint16 -> b |> set union_tag 7
        | Uint32 -> b |> set union_tag 8
        | Uint64 -> b |> set union_tag 9
        | Float32 -> b |> set union_tag 10
        | Float64 -> b |> set union_tag 11
        | Text -> b |> set union_tag 12
        | Data -> b |> set union_tag 13
        | List x -> b |> set union_tag 14 |> set (group t @@ Ptr Types.Type.List.t) x
        | Enum x -> b |> set union_tag 15 |> set (group t @@ Ptr Types.Type.Enum.t) x
        | Struct x -> b |> set union_tag 16 |> set (group t @@ Ptr Types.Type.Struct.t) x
        | Interface x -> b |> set union_tag 17 |> set (group t @@ Ptr Types.Type.Interface.t) x
        | AnyPointer x -> b |> set union_tag 18 |> set (group t @@ Ptr Types.Type.AnyPointer.t) x
      in ug f g
  end
  
  module Brand = struct
    type t = Types.Brand.t
    let t = Types.Brand.t
    module Scope = struct
      type t = Types.Brand.Scope.t
      let t = Types.Brand.Scope.t
      
      (* Field definitions *)
      let scopeId = field t UInt64 0l
      
      (* Unnamed union *)
      
      type union =
        | Bind of Types.Brand.Binding.t s c array
        | Inherit
      let union =
        let union_tag = field t UInt16 64l in
        let f c =
          match c |> get union_tag with
            | 0 -> Bind (get (field t (List (ptr Types.Brand.Binding.t)) 0l) c)
            | 1 -> Inherit
            | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
        in let g b = function
          | Bind x -> b |> set union_tag 0 |> set (field t (List (ptr Types.Brand.Binding.t)) 0l) x
          | Inherit -> b |> set union_tag 1
        in ug f g
    end
    
    module Binding = struct
      type t = Types.Brand.Binding.t
      let t = Types.Brand.Binding.t
      
      (* Field definitions *)
      
      (* Unnamed union *)
      
      type union =
        | Unbound
        | Type of Types.Type.t s c
      let union =
        let union_tag = field t UInt16 0l in
        let f c =
          match c |> get union_tag with
            | 0 -> Unbound
            | 1 -> Type (get (field t (ptr Types.Type.t) 0l) c)
            | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
        in let g b = function
          | Unbound -> b |> set union_tag 0
          | Type x -> b |> set union_tag 1 |> set (field t (ptr Types.Type.t) 0l) x
        in ug f g
    end
    
    
    (* Field definitions *)
    let scopes = field t (List (ptr Types.Brand.Scope.t)) 0l
  end
  
  module Value = struct
    type t = Types.Value.t
    let t = Types.Value.t
    
    (* Field definitions *)
    
    (* Unnamed union *)
    
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
        match c |> get union_tag with
          | 0 -> Void
          | 1 -> Bool (get (field t Bool 16l) c)
          | 2 -> Int8 (get (field t Int8 16l) c)
          | 3 -> Int16 (get (field t Int16 16l) c)
          | 4 -> Int32 (get (field t Int32 32l) c)
          | 5 -> Int64 (get (field t Int64 64l) c)
          | 6 -> Uint8 (get (field t UInt8 16l) c)
          | 7 -> Uint16 (get (field t UInt16 16l) c)
          | 8 -> Uint32 (get (field t UInt32 32l) c)
          | 9 -> Uint64 (get (field t UInt64 64l) c)
          | 10 -> Float32 (get (field t Float32 32l) c)
          | 11 -> Float64 (get (field t Float64 64l) c)
          | 12 -> Text (get (field t Text 0l) c)
          | 13 -> Data (get (field t Data 0l) c)
          | 14 -> List (get (field t (Ptr Void) 0l) c)
          | 15 -> Enum (get (field t UInt16 16l) c)
          | 16 -> Struct (get (field t (Ptr Void) 0l) c)
          | 17 -> Interface
          | 18 -> AnyPointer (get (field t (Ptr Void) 0l) c)
          | n -> failwith @@ Printf.sprintf "Invalid union tag: %d" n
      in let g b = function
        | Void -> b |> set union_tag 0
        | Bool x -> b |> set union_tag 1 |> set (field t Bool 16l) x
        | Int8 x -> b |> set union_tag 2 |> set (field t Int8 16l) x
        | Int16 x -> b |> set union_tag 3 |> set (field t Int16 16l) x
        | Int32 x -> b |> set union_tag 4 |> set (field t Int32 32l) x
        | Int64 x -> b |> set union_tag 5 |> set (field t Int64 64l) x
        | Uint8 x -> b |> set union_tag 6 |> set (field t UInt8 16l) x
        | Uint16 x -> b |> set union_tag 7 |> set (field t UInt16 16l) x
        | Uint32 x -> b |> set union_tag 8 |> set (field t UInt32 32l) x
        | Uint64 x -> b |> set union_tag 9 |> set (field t UInt64 64l) x
        | Float32 x -> b |> set union_tag 10 |> set (field t Float32 32l) x
        | Float64 x -> b |> set union_tag 11 |> set (field t Float64 64l) x
        | Text x -> b |> set union_tag 12 |> set (field t Text 0l) x
        | Data x -> b |> set union_tag 13 |> set (field t Data 0l) x
        | List x -> b |> set union_tag 14 |> set (field t (Ptr Void) 0l) x
        | Enum x -> b |> set union_tag 15 |> set (field t UInt16 16l) x
        | Struct x -> b |> set union_tag 16 |> set (field t (Ptr Void) 0l) x
        | Interface -> b |> set union_tag 17
        | AnyPointer x -> b |> set union_tag 18 |> set (field t (Ptr Void) 0l) x
      in ug f g
  end
  
  module Annotation = struct
    type t = Types.Annotation.t
    let t = Types.Annotation.t
    
    (* Field definitions *)
    let id = field t UInt64 0l
    let value = field t (ptr Types.Value.t) 0l
    let brand = field t (ptr Types.Brand.t) 1l
  end
  
  module CapnpVersion = struct
    type t = Types.CapnpVersion.t
    let t = Types.CapnpVersion.t
    
    (* Field definitions *)
    let major = field t UInt16 0l
    let minor = field t UInt8 16l
    let micro = field t UInt8 24l
  end
  
  module CodeGeneratorRequest = struct
    type t = Types.CodeGeneratorRequest.t
    let t = Types.CodeGeneratorRequest.t
    module RequestedFile = struct
      type t = Types.CodeGeneratorRequest.RequestedFile.t
      let t = Types.CodeGeneratorRequest.RequestedFile.t
      module Import = struct
        type t = Types.CodeGeneratorRequest.RequestedFile.Import.t
        let t = Types.CodeGeneratorRequest.RequestedFile.Import.t
        
        (* Field definitions *)
        let id = field t UInt64 0l
        let name = field t Text 0l
      end
      
      
      (* Field definitions *)
      let id = field t UInt64 0l
      let filename = field t Text 0l
      let imports = field t (List (ptr Types.CodeGeneratorRequest.RequestedFile.Import.t)) 1l
    end
    
    
    (* Field definitions *)
    let nodes = field t (List (ptr Types.Node.t)) 0l
    let requestedFiles = field t (List (ptr Types.CodeGeneratorRequest.RequestedFile.t)) 1l
    let capnpVersion = field t (ptr Types.CapnpVersion.t) 2l
    let sourceInfo = field t (List (ptr Types.Node.SourceInfo.t)) 3l
  end
end

include Decls

