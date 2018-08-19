

(* This file was generated by capnptk. It is probably not a good idea to edit
   it. *) 
open Capnptk.Declarative

module Self = struct
  module LinkedList = struct
    type t' type t = t' sgu let t : t g = sg 2 1 end
  module FooServer = struct
    type t' type t = t' igu let t : t g = ig 2 0xdd26071fa8e78974L
  end
  module BarServer = struct
    type t' type t = t' igu let t : t g = ig 1 0xa882934f19d864ecL
  end
end

(* Now we begin the body. This is what will be referenced by outside scripts,
   so the first thing we do in each module is alias to the types declared
   above.  *) 



module LinkedList = struct
  include (Self.LinkedList : Type with type t = Self.LinkedList.t)
  
  type union =
    | End
    | Cons of Self.LinkedList.t
  let union =
    let union_tag = field t UInt16 64l in
    let f c =
      match c => union_tag with
        | 0 -> End
        | 1 -> Cons (c => (field t Self.LinkedList.t 0l))
        | n -> raise (Capnptk.OrdinalError n)
    in
    let g b = function
      | End -> b |> set union_tag 0;
      | Cons v -> b |> set (field t Self.LinkedList.t 0l) v |> set union_tag 1
    in ug f g
  let payload = field t Int64 0l
end
module FooServer = struct
  include (Self.FooServer : Type with type t = Self.FooServer.t)
  module Get1_Params = struct
    type t' type t = t' sgu let t : t g = sg 0 0
  end
  module Get1_Results = struct
    type t' type t = t' sgu let t : t g = sg 0 1
    let result = field t Self.BarServer.t 0l
  end
  module Get2_Params = struct
    type t' type t = t' sgu let t : t g = sg 0 0
  end
  module Get2_Results = struct
    type t' type t = t' sgu let t : t g = sg 0 1
    let result = field t Self.BarServer.t 0l
  end
  let get1 = defmethod t Get1_Params.t Get1_Results.t 0 "get1" 
  let get2 = defmethod t Get2_Params.t Get2_Results.t 1 "get2" 
end
module BarServer = struct
  include (Self.BarServer : Type with type t = Self.BarServer.t)
  module Get_Params = struct
    type t' type t = t' sgu let t : t g = sg 0 0 end
  module Get_Results = struct
    type t' type t = t' sgu let t : t g = sg 0 1
    let result = field t Text 0l
  end
  let get = defmethod t Get_Params.t Get_Results.t 0 "get" 
end

