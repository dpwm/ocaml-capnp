module Functors = struct
  open Declarative
  module UInt8 : Type  = struct type t = int let t = UInt8 end
  module Int8 : Type   = struct type t = int let t = Int8 end
  module UInt16 : Type = struct type t = int let t = UInt16 end
  module Int16 : Type  = struct type t = int let t = Int16 end
  module UInt32 : Type = struct type t = int32 let t = UInt32 end
  module Int32 : Type  = struct type t = int32 let t = Int32 end
  module UInt64 : Type = struct type t = int64 let t = UInt64 end
  module Int64 : Type  = struct type t = int64 let t = Int64 end
  module Float64 : Type = struct type t = float let t = Float64 end
  module Float32 : Type = struct type t = float let t = Float32 end
  module Bool : Type   = struct type t = bool let t = Bool end
  module Text : Type   = struct type t = string let t = Data end
  module Data : Type   = struct type t = string let t = Data end
  module List(T: Type) : Type = struct type t = T.t array let t = List T.t end
  module Struct(T: Type) : Type = struct type t = T.t let t = T.t end
end
