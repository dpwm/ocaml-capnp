

(* This file was generated by capnptk. It is probably not a good idea to edit
   it. *) 
open Capnptk.Declarative

module Self = struct
  module Message = struct
    type t' type t = t' sgu let t : t g = sg 1 1 end
  module Bootstrap = struct
    type t' type t = t' sgu let t : t g = sg 1 1 end
  module Call = struct
    type t' type t = t' sgu let t : t g = sg 3 3
    module SendResultsTo = struct
      type t' type t = t' sgu let t : t g = sg 3 3
    end
  end
  module Return = struct
    type t' type t = t' sgu let t : t g = sg 2 1 end
  module Finish = struct
    type t' type t = t' sgu let t : t g = sg 1 0 end
  module Resolve = struct
    type t' type t = t' sgu let t : t g = sg 1 1 end
  module Release = struct
    type t' type t = t' sgu let t : t g = sg 1 0 end
  module Disembargo = struct
    type t' type t = t' sgu let t : t g = sg 1 1
    module Context = struct
      type t' type t = t' sgu let t : t g = sg 1 1 end
  end
  module Provide = struct
    type t' type t = t' sgu let t : t g = sg 1 2 end
  module Accept = struct
    type t' type t = t' sgu let t : t g = sg 1 1 end
  module Join = struct
    type t' type t = t' sgu let t : t g = sg 1 2 end
  module MessageTarget = struct
    type t' type t = t' sgu let t : t g = sg 1 1
  end
  module Payload = struct
    type t' type t = t' sgu let t : t g = sg 0 2 end
  module CapDescriptor = struct
    type t' type t = t' sgu let t : t g = sg 1 1
  end
  module PromisedAnswer = struct
    type t' type t = t' sgu let t : t g = sg 1 1
    module Op = struct
      type t' type t = t' sgu let t : t g = sg 1 0 end
  end
  module ThirdPartyCapDescriptor = struct
    type t' type t = t' sgu let t : t g = sg 1 1
  end
  module Exception = struct
    type t' type t = t' sgu let t : t g = sg 1 1
    module Type = struct
      type t = | Failed | Overloaded | Disconnected | Unimplemented let t =
      let f = function | 0 -> Failed | 1 -> Overloaded | 2 -> Disconnected
      | 3 -> Unimplemented | n -> raise (Capnptk.OrdinalError n) in let g =
      function | Failed -> 0 | Overloaded -> 1 | Disconnected -> 2
      | Unimplemented -> 3 in Enum(f, g)
    end
  end
end

(* Now we begin the body. This is what will be referenced by outside scripts,
   so the first thing we do in each module is alias to the types declared
   above.  *) 



module Message = struct
  include (Self.Message : Type with type t = Self.Message.t)
  
  type union =
    | Unimplemented of Self.Message.t
    | Abort of Self.Exception.t
    | Call of Self.Call.t
    | Return of Self.Return.t
    | Finish of Self.Finish.t
    | Resolve of Self.Resolve.t
    | Release of Self.Release.t
    | ObsoleteSave of unit c
    | Bootstrap of Self.Bootstrap.t
    | ObsoleteDelete of unit c
    | Provide of Self.Provide.t
    | Accept of Self.Accept.t
    | Join of Self.Join.t
    | Disembargo of Self.Disembargo.t
  let union =
    let union_tag = field t UInt16 0l in
    let f c =
      match c => union_tag with
        | 0 -> Unimplemented (c => (field t Self.Message.t 0l))
        | 1 -> Abort (c => (field t Self.Exception.t 0l))
        | 2 -> Call (c => (field t Self.Call.t 0l))
        | 3 -> Return (c => (field t Self.Return.t 0l))
        | 4 -> Finish (c => (field t Self.Finish.t 0l))
        | 5 -> Resolve (c => (field t Self.Resolve.t 0l))
        | 6 -> Release (c => (field t Self.Release.t 0l))
        | 7 -> ObsoleteSave (c => (field t (Ptr Void) 0l))
        | 8 -> Bootstrap (c => (field t Self.Bootstrap.t 0l))
        | 9 -> ObsoleteDelete (c => (field t (Ptr Void) 0l))
        | 10 -> Provide (c => (field t Self.Provide.t 0l))
        | 11 -> Accept (c => (field t Self.Accept.t 0l))
        | 12 -> Join (c => (field t Self.Join.t 0l))
        | 13 -> Disembargo (c => (field t Self.Disembargo.t 0l))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | Unimplemented v -> b |> set (field t Self.Message.t 0l) v |> set union_tag 0
      | Abort v -> b |> set (field t Self.Exception.t 0l) v |> set union_tag 1
      | Call v -> b |> set (field t Self.Call.t 0l) v |> set union_tag 2
      | Return v -> b |> set (field t Self.Return.t 0l) v |> set union_tag 3
      | Finish v -> b |> set (field t Self.Finish.t 0l) v |> set union_tag 4
      | Resolve v -> b |> set (field t Self.Resolve.t 0l) v |> set union_tag 5
      | Release v -> b |> set (field t Self.Release.t 0l) v |> set union_tag 6
      | ObsoleteSave v -> b |> set (field t (Ptr Void) 0l) v |> set union_tag 7
      | Bootstrap v -> b |> set (field t Self.Bootstrap.t 0l) v |> set union_tag 8
      | ObsoleteDelete v -> b |> set (field t (Ptr Void) 0l) v |> set union_tag 9
      | Provide v -> b |> set (field t Self.Provide.t 0l) v |> set union_tag 10
      | Accept v -> b |> set (field t Self.Accept.t 0l) v |> set union_tag 11
      | Join v -> b |> set (field t Self.Join.t 0l) v |> set union_tag 12
      | Disembargo v -> b |> set (field t Self.Disembargo.t 0l) v |> set union_tag 13
    in ug f g
end

module Bootstrap = struct
  include (Self.Bootstrap : Type with type t = Self.Bootstrap.t)
  let questionId = field t UInt32 0l
  let deprecatedObjectId = field t (Ptr Void) 0l
end

module Call = struct
  include (Self.Call : Type with type t = Self.Call.t)
  
  module SendResultsTo = struct
    include (Self.Call.SendResultsTo : Type with type t = Self.Call.SendResultsTo.t)
    
    type union =
      | Caller
      | Yourself
      | ThirdParty of unit c
    let union =
      let union_tag = field t UInt16 48l in
      let f c =
        match c => union_tag with
          | 0 -> Caller
          | 1 -> Yourself
          | 2 -> ThirdParty (c => (field t (Ptr Void) 2l))
          | n -> raise (Capnptk.OrdinalError n)
      in
      let g b = function
        | Caller -> b |> set union_tag 0;
        | Yourself -> b |> set union_tag 1;
        | ThirdParty v -> b |> set (field t (Ptr Void) 2l) v |> set union_tag 2
      in ug f g
  end
  let questionId = field t UInt32 0l
  let target = field t Self.MessageTarget.t 0l
  let interfaceId = field t UInt64 64l
  let methodId = field t UInt16 32l
  let params = field t Self.Payload.t 1l
  let sendResultsTo = group t (Self.Call.SendResultsTo.t)
  let allowThirdPartyTailCall = field t Bool 128l
end

module Return = struct
  include (Self.Return : Type with type t = Self.Return.t)
  
  type union =
    | Results of Self.Payload.t
    | Exception of Self.Exception.t
    | Canceled
    | ResultsSentElsewhere
    | TakeFromOtherQuestion of int32
    | AcceptFromThirdParty of unit c
  let union =
    let union_tag = field t UInt16 48l in
    let f c =
      match c => union_tag with
        | 0 -> Results (c => (field t Self.Payload.t 0l))
        | 1 -> Exception (c => (field t Self.Exception.t 0l))
        | 2 -> Canceled
        | 3 -> ResultsSentElsewhere
        | 4 -> TakeFromOtherQuestion (c => (field t UInt32 64l))
        | 5 -> AcceptFromThirdParty (c => (field t (Ptr Void) 0l))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | Results v -> b |> set (field t Self.Payload.t 0l) v |> set union_tag 0
      | Exception v -> b |> set (field t Self.Exception.t 0l) v |> set union_tag 1
      | Canceled -> b |> set union_tag 2;
      | ResultsSentElsewhere -> b |> set union_tag 3;
      | TakeFromOtherQuestion v -> b |> set (field t UInt32 64l) v |> set union_tag 4
      | AcceptFromThirdParty v -> b |> set (field t (Ptr Void) 0l) v |> set union_tag 5
    in ug f g
  let answerId = field t UInt32 0l
  let releaseParamCaps = field t Bool ~default:true 32l
end

module Finish = struct
  include (Self.Finish : Type with type t = Self.Finish.t)
  let questionId = field t UInt32 0l
  let releaseResultCaps = field t Bool ~default:true 32l
end

module Resolve = struct
  include (Self.Resolve : Type with type t = Self.Resolve.t)
  
  type union =
    | Cap of Self.CapDescriptor.t
    | Exception of Self.Exception.t
  let union =
    let union_tag = field t UInt16 32l in
    let f c =
      match c => union_tag with
        | 0 -> Cap (c => (field t Self.CapDescriptor.t 0l))
        | 1 -> Exception (c => (field t Self.Exception.t 0l))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | Cap v -> b |> set (field t Self.CapDescriptor.t 0l) v |> set union_tag 0
      | Exception v -> b |> set (field t Self.Exception.t 0l) v |> set union_tag 1
    in ug f g
  let promiseId = field t UInt32 0l
end

module Release = struct
  include (Self.Release : Type with type t = Self.Release.t)
  let id = field t UInt32 0l
  let referenceCount = field t UInt32 32l
end

module Disembargo = struct
  include (Self.Disembargo : Type with type t = Self.Disembargo.t)
  
  module Context = struct
    include (Self.Disembargo.Context : Type with type t = Self.Disembargo.Context.t)
    
    type union =
      | SenderLoopback of int32
      | ReceiverLoopback of int32
      | Accept
      | Provide of int32
    let union =
      let union_tag = field t UInt16 32l in
      let f c =
        match c => union_tag with
          | 0 -> SenderLoopback (c => (field t UInt32 0l))
          | 1 -> ReceiverLoopback (c => (field t UInt32 0l))
          | 2 -> Accept
          | 3 -> Provide (c => (field t UInt32 0l))
          | n -> raise (Capnptk.OrdinalError n)
      in
      let g b = function
        | SenderLoopback v -> b |> set (field t UInt32 0l) v |> set union_tag 0
        | ReceiverLoopback v -> b |> set (field t UInt32 0l) v |> set union_tag 1
        | Accept -> b |> set union_tag 2;
        | Provide v -> b |> set (field t UInt32 0l) v |> set union_tag 3
      in ug f g
  end
  let target = field t Self.MessageTarget.t 0l
  let context = group t (Self.Disembargo.Context.t)
end

module Provide = struct
  include (Self.Provide : Type with type t = Self.Provide.t)
  let questionId = field t UInt32 0l
  let target = field t Self.MessageTarget.t 0l
  let recipient = field t (Ptr Void) 1l
end

module Accept = struct
  include (Self.Accept : Type with type t = Self.Accept.t)
  let questionId = field t UInt32 0l
  let provision = field t (Ptr Void) 0l
  let embargo = field t Bool 32l
end

module Join = struct
  include (Self.Join : Type with type t = Self.Join.t)
  let questionId = field t UInt32 0l
  let target = field t Self.MessageTarget.t 0l
  let keyPart = field t (Ptr Void) 1l
end

module MessageTarget = struct
  include (Self.MessageTarget : Type with type t = Self.MessageTarget.t)
  
  type union =
    | ImportedCap of int32
    | PromisedAnswer of Self.PromisedAnswer.t
  let union =
    let union_tag = field t UInt16 32l in
    let f c =
      match c => union_tag with
        | 0 -> ImportedCap (c => (field t UInt32 0l))
        | 1 -> PromisedAnswer (c => (field t Self.PromisedAnswer.t 0l))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | ImportedCap v -> b |> set (field t UInt32 0l) v |> set union_tag 0
      | PromisedAnswer v -> b |> set (field t Self.PromisedAnswer.t 0l) v |> set union_tag 1
    in ug f g
end

module Payload = struct
  include (Self.Payload : Type with type t = Self.Payload.t)
  let content = field t (Ptr Void) 0l
  let capTable = field t (List Self.CapDescriptor.t) 1l
end

module CapDescriptor = struct
  include (Self.CapDescriptor : Type with type t = Self.CapDescriptor.t)
  
  type union =
    | None
    | SenderHosted of int32
    | SenderPromise of int32
    | ReceiverHosted of int32
    | ReceiverAnswer of Self.PromisedAnswer.t
    | ThirdPartyHosted of Self.ThirdPartyCapDescriptor.t
  let union =
    let union_tag = field t UInt16 0l in
    let f c =
      match c => union_tag with
        | 0 -> None
        | 1 -> SenderHosted (c => (field t UInt32 32l))
        | 2 -> SenderPromise (c => (field t UInt32 32l))
        | 3 -> ReceiverHosted (c => (field t UInt32 32l))
        | 4 -> ReceiverAnswer (c => (field t Self.PromisedAnswer.t 0l))
        | 5 -> ThirdPartyHosted (c => (field t Self.ThirdPartyCapDescriptor.t 0l))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | None -> b |> set union_tag 0;
      | SenderHosted v -> b |> set (field t UInt32 32l) v |> set union_tag 1
      | SenderPromise v -> b |> set (field t UInt32 32l) v |> set union_tag 2
      | ReceiverHosted v -> b |> set (field t UInt32 32l) v |> set union_tag 3
      | ReceiverAnswer v -> b |> set (field t Self.PromisedAnswer.t 0l) v |> set union_tag 4
      | ThirdPartyHosted v -> b |> set (field t Self.ThirdPartyCapDescriptor.t 0l) v |> set union_tag 5
    in ug f g
end

module PromisedAnswer = struct
  include (Self.PromisedAnswer : Type with type t = Self.PromisedAnswer.t)
  
  module Op = struct
    include (Self.PromisedAnswer.Op : Type with type t = Self.PromisedAnswer.Op.t)
    
    type union =
      | Noop
      | GetPointerField of int
    let union =
      let union_tag = field t UInt16 0l in
      let f c =
        match c => union_tag with
          | 0 -> Noop
          | 1 -> GetPointerField (c => (field t UInt16 16l))
          | n -> raise (Capnptk.OrdinalError n)
      in
      let g b = function
        | Noop -> b |> set union_tag 0;
        | GetPointerField v -> b |> set (field t UInt16 16l) v |> set union_tag 1
      in ug f g
  end
  let questionId = field t UInt32 0l
  let transform = field t (List Self.PromisedAnswer.Op.t) 0l
end

module ThirdPartyCapDescriptor = struct
  include (Self.ThirdPartyCapDescriptor : Type with type t = Self.ThirdPartyCapDescriptor.t)
  let id = field t (Ptr Void) 0l
  let vineId = field t UInt32 0l
end

module Exception = struct
  include (Self.Exception : Type with type t = Self.Exception.t)
  
  module Type = struct
    include (Self.Exception.Type : Type with type t = Self.Exception.Type.t)
  end
  let reason = field t Text 0l
  let obsoleteIsCallersFault = field t Bool 0l
  let obsoleteDurability = field t UInt16 16l
  let type_ = field t Self.Exception.Type.t 32l
end

