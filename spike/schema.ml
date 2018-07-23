(* schema.capnp *)

(* This file was auto-generated from a capnproto schema.
   You should not need to edit this file!                *)

(* The Types module pre-defines all the types used. 
   This simplifies implementation by acting as forward definitions.  *)

module Types = struct
  
  open Capnptk.Declarative
  
  module Node = struct
    type t
    let t : t sg = sg
    
    module Parameter = struct
      type t
      let t : t sg = sg
    end
    
    module NestedNode = struct
      type t
      let t : t sg = sg
    end
    
    module SourceInfo = struct
      type t
      let t : t sg = sg
      
      module Member = struct
        type t
        let t : t sg = sg
      end
    end
    
    module Struct = struct
      type t
      let t : t sg = sg
    end
    
    module Enum = struct
      type t
      let t : t sg = sg
    end
    
    module Interface = struct
      type t
      let t : t sg = sg
    end
    
    module Const = struct
      type t
      let t : t sg = sg
    end
    
    module Annotation = struct
      type t
      let t : t sg = sg
    end
  end
  
  module Field = struct
    type t
    let t : t sg = sg
    
    module Slot = struct
      type t
      let t : t sg = sg
    end
    
    module Group = struct
      type t
      let t : t sg = sg
    end
    
    module Ordinal = struct
      type t
      let t : t sg = sg
    end
  end
  
  module Enumerant = struct
    type t
    let t : t sg = sg
  end
  
  module Superclass = struct
    type t
    let t : t sg = sg
  end
  
  module Method = struct
    type t
    let t : t sg = sg
  end
  
  module Type = struct
    type t
    let t : t sg = sg
    
    module List = struct
      type t
      let t : t sg = sg
    end
    
    module Enum = struct
      type t
      let t : t sg = sg
    end
    
    module Struct = struct
      type t
      let t : t sg = sg
    end
    
    module Interface = struct
      type t
      let t : t sg = sg
    end
    
    module AnyPointer = struct
      type t
      let t : t sg = sg
      
      module Unconstrained = struct
        type t
        let t : t sg = sg
      end
      
      module Parameter = struct
        type t
        let t : t sg = sg
      end
      
      module ImplicitMethodParameter = struct
        type t
        let t : t sg = sg
      end
    end
  end
  
  module Brand = struct
    type t
    let t : t sg = sg
    
    module Scope = struct
      type t
      let t : t sg = sg
    end
    
    module Binding = struct
      type t
      let t : t sg = sg
    end
  end
  
  module Value = struct
    type t
    let t : t sg = sg
  end
  
  module Annotation = struct
    type t
    let t : t sg = sg
  end
  
  module CapnpVersion = struct
    type t
    let t : t sg = sg
  end
  
  module CodeGeneratorRequest = struct
    type t
    let t : t sg = sg
    
    module RequestedFile = struct
      type t
      let t : t sg = sg
      
      module Import = struct
        type t
        let t : t sg = sg
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
      let dataWordCount = field t UInt16 7l
      let pointerCount = field t UInt16 12l
      let preferredListEncoding = field t (UInt16) 13l
      let isGroup = field t Bool 224l
      let discriminantCount = field t UInt16 15l
      let discriminantOffset = field t UInt32 8l
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
    let displayNamePrefixLength = field t UInt32 2l
    let scopeId = field t UInt64 2l
    let nestedNodes = field t (List (ptr Types.Node.NestedNode.t)) 1l
    let annotations = field t (List (ptr Types.Annotation.t)) 2l
    let parameters = field t (List (ptr Types.Node.Parameter.t)) 5l
    let isGeneric = field t Bool 288l
    
    (* Unnamed union *)
    
    type union =
      | File
      | Struct of Types.Node.Struct.t c
      | Enum of Types.Node.Enum.t c
      | Interface of Types.Node.Interface.t c
      | Const of Types.Node.Const.t c
      | Annotation of Types.Node.Annotation.t c
  end
  
  module Field = struct
    type t = Types.Field.t
    let t = Types.Field.t
    module Slot = struct
      type t = Types.Field.Slot.t
      let t = Types.Field.Slot.t
      
      (* Field definitions *)
      let offset = field t UInt32 1l
      let type_ = field t (ptr Types.Type.t) 2l
      let defaultValue = field t (ptr Types.Value.t) 3l
      let hadExplicitDefault = field t Bool 128l
    end
    
    module Group = struct
      type t = Types.Field.Group.t
      let t = Types.Field.Group.t
      
      (* Field definitions *)
      let typeId = field t UInt64 2l
    end
    
    module Ordinal = struct
      type t = Types.Field.Ordinal.t
      let t = Types.Field.Ordinal.t
      
      (* Field definitions *)
      
      (* Unnamed union *)
      
      type union =
        | Implicit
        | Explicit of int
    end
    
    
    (* Field definitions *)
    let name = field t Text 0l
    let codeOrder = field t UInt16 0l
    let annotations = field t (List (ptr Types.Annotation.t)) 1l
    let discriminantValue = field t UInt16 1l
    let ordinal = group t (ptr Types.Field.Ordinal.t)
    
    (* Unnamed union *)
    
    type union =
      | Slot of Types.Field.Slot.t c
      | Group of Types.Field.Group.t c
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
    let paramStructType = field t UInt64 1l
    let resultStructType = field t UInt64 2l
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
      let typeId = field t UInt64 1l
      let brand = field t (ptr Types.Brand.t) 0l
    end
    
    module Struct = struct
      type t = Types.Type.Struct.t
      let t = Types.Type.Struct.t
      
      (* Field definitions *)
      let typeId = field t UInt64 1l
      let brand = field t (ptr Types.Brand.t) 0l
    end
    
    module Interface = struct
      type t = Types.Type.Interface.t
      let t = Types.Type.Interface.t
      
      (* Field definitions *)
      let typeId = field t UInt64 1l
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
      end
      
      module Parameter = struct
        type t = Types.Type.AnyPointer.Parameter.t
        let t = Types.Type.AnyPointer.Parameter.t
        
        (* Field definitions *)
        let scopeId = field t UInt64 2l
        let parameterIndex = field t UInt16 5l
      end
      
      module ImplicitMethodParameter = struct
        type t = Types.Type.AnyPointer.ImplicitMethodParameter.t
        let t = Types.Type.AnyPointer.ImplicitMethodParameter.t
        
        (* Field definitions *)
        let parameterIndex = field t UInt16 5l
      end
      
      
      (* Field definitions *)
      
      (* Unnamed union *)
      
      type union =
        | Unconstrained of Types.Type.AnyPointer.Unconstrained.t c
        | Parameter of Types.Type.AnyPointer.Parameter.t c
        | ImplicitMethodParameter of Types.Type.AnyPointer.ImplicitMethodParameter.t c
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
      | List of Types.Type.List.t c
      | Enum of Types.Type.Enum.t c
      | Struct of Types.Type.Struct.t c
      | Interface of Types.Type.Interface.t c
      | AnyPointer of Types.Type.AnyPointer.t c
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
        | Bind of Types.Brand.Binding.t array
        | Inherit
    end
    
    module Binding = struct
      type t = Types.Brand.Binding.t
      let t = Types.Brand.Binding.t
      
      (* Field definitions *)
      
      (* Unnamed union *)
      
      type union =
        | Unbound
        | Type of Types.Type.t
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
      | List of AnyPointer.t
      | Enum of int
      | Struct of AnyPointer.t
      | Interface
      | AnyPointer of AnyPointer.t
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
    let minor = field t UInt8 2l
    let micro = field t UInt8 3l
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

