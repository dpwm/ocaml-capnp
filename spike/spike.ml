open Bigarray

let f = Unix.openfile "dump.bin" [O_RDONLY] 0o640

let a = Unix.map_file f int8_unsigned c_layout false [| -1 |] |> array1_of_genarray

type data_t = (int, int8_unsigned_elt, c_layout) Array1.t


let split ns x =
  let rec f x = function
    | (n :: ns) ->
        Int64.((shift_left 1L n |> (add (-1L)) |> logand x) :: f (shift_right_logical x n) ns)
    | [] -> []
  in
  f x ns

let sptr = [2; 30; 16; 16]
let lptr = [2; 30; 3; 29]
let fptr = [2; 1; 29; 32]
let cptr = [2; 30; 32]

type 'a cursor = {
  pos : int;
  data : data_t;
  output: 'a;
  dwords: int;
  pwords: int;
  frames: int array;
}

module Decl = struct

  type 'a struct_data = StructData


  type 'a t =
    | Void : unit t
    | Bool : bool t

    | Int8 : int t
    | Int16 : int t
    | Int32 : int32 t
    | Int64 : int64 t

    | UInt8 : int t
    | UInt16 : int t
    | UInt32 : int32 t
    | UInt64 : int64 t

    | Struct : 'a struct_data t

    | List : 'a t -> 'a cursor array t

    | Text : string t
    | Data : string t

  type 'a structure = 'a struct_data t
  type ('t, 'a) field = Field of ('a t * int)

  let void = Void
  let bool = Bool
  let int8 = Int8
  let int16 = Int16
  let int32 = Int32
  let int64 = Int64

  let uint8 = UInt8
  let uint16 = UInt16
  let uint32 = UInt32
  let uint64 = UInt64

  let text = Text
  let data = Data

  let structure = Struct 

  let list a = List a

  let field: type a. int -> a t -> 't t -> ('t, a) field = 
    fun n t2 t1 -> Field (t2, n)

end

(* This is really attempt 1 *)




let outputmap f p = {p with output = f p.output}
let mov n p = {p with pos = n + p.pos}
let output output p = {p with output}
let result {output} = output

let read_int8 p = {p with pos = p.pos + 1; output=p.data.{p.pos}}
let read_int16 p = {p with pos = p.pos + 2; output=p.data.{p.pos} + (p.data.{p.pos + 1} lsl 8)}
let read_int32 ({pos; data} as p) = 
  let a = data.{pos} + (data.{pos + 1} lsl 8) in
  let b = data.{pos + 2} + (data.{pos + 3} lsl 8) in
  let output = Int32.(add (of_int a) (shift_left (of_int b) 16)) in
  {p with data; pos = pos + 4; output}

let read_int64 ({pos; data} as p) = 
  let a = data.{pos} + (data.{pos + 1} lsl 8) + (data.{pos + 2} lsl 16) in
  let b = data.{pos + 3} + (data.{pos + 4} lsl 8) + (data.{pos + 5} lsl 16) in
  let c = data.{pos + 6} + (data.{pos + 7} lsl 8) in
  let output = Int64.(add (of_int a) (shift_left (add (of_int b) (shift_left (of_int c) 24)) 24)) in
  {p with data; pos = pos + 8; output}


let movbits n p = mov (n / 8) p

let ptr n c =
  let c = c |> mov ((c.dwords + n) * 8) |> output 0L |> ref in

  let continue = ref true in

  while !continue do
    let c' = !c |> read_int64 in
    match c' |> result |> split fptr with
    | [2L; n; offset; segment] -> 
        let offset = Int64.to_int offset in
        let segment = Int64.to_int segment in
        let c' = {c' with pos=c'.frames.(segment) + offset * 8} in
        match n with
        | 0L ->
            c := c'
        | 1L ->
            (* This means that the landing pointer is actually two words. The first contains the details of the target, the second *)
            c'  |> read_int64 |> result |> split fptr |> List.iter (Printf.printf "%Lu\n");
            c'  |> read_int64 |> read_int64 |> result |> split fptr |> List.iter (Printf.printf "%Lu\n");
            failwith "Rare!"
        | _ -> failwith "Broken pointer" ; ;
    | _ ->
        continue := false
  done;

  !c


let rec get : type a. ('b, a) Decl.field -> 'b cursor -> a cursor =
  let open Decl in
  fun (Field (t, n)) c ->
    let c' = movbits n c in
    match t with
      | Void -> c |> output ()

      | Bool ->
          let n = n mod 8 in
          c' |> read_int8 |> outputmap (
            fun x -> 0 <> x land (1 lsl n))

      | Int8 ->
          c' |> read_int8 |> outputmap (fun x -> if x > 127 then x - 256 else x)
      | Int16 ->
          c' |> read_int16 |> outputmap (fun x -> if x > 32768 then x - 65536 else x)
      | Int32 -> 
          c' |> read_int32
      | Int64 -> 
          c' |> read_int64

      | UInt8 -> c' |> read_int8
      | UInt16 -> c' |> read_int16
      | UInt32 -> c' |> read_int32
      | UInt64-> c' |> read_int64

      | Struct -> 
          let c = c |> ptr n |> read_int64 in

          match (c.output |> split sptr |> List.map (Int64.to_int)) with
          | [0; offset; dwords; pwords] ->
              c |> mov (8 * offset) |> fun c -> 
                {c with output=StructData; dwords; pwords}
          | _ -> failwith "not a struct pointer" ; ;

      | List Struct ->
          let c = c |> ptr n |> read_int64 in
          c |> result |> split lptr |> List.map Int64.to_int |>
          begin function
            | [1; offset; 7; words] ->
                let c = c |> mov (8 * offset) |> read_int64 in
                c |> result |> split sptr |> List.map Int64.to_int |> function
                  | [0; nelem; dwords; pwords] ->
                      let c0 = {c with dwords; pwords; output=StructData} in
                      let wsize = (dwords + pwords) * 8 in
                      let output = Array.make nelem c0 in
                      for i = 0 to nelem - 1 do
                        output.(i) <- c0 |> mov (i * wsize)
                      done;
                      {c0 with output}
                  | _ -> failwith "not implemented yet" ; ;
            | _ -> failwith "not implemented yet"

            end

      | List Bool -> c |> output [| |]
      | List x -> c |> output [| |]

      | Text ->
          let c = c |> ptr n |> read_int64 in
          c |> result |> split lptr |> List.map Int64.to_int |> (function
            | [1; offset; 2; elements] ->
                let c = c |> mov (offset * 8) in
                let bs = Bytes.create (elements - 1) in
                for i = 0 to elements - 2 do
                  Bytes.set bs i (Char.chr c.data.{c.pos+i})
                done;
                
                (* This is an OK use of unsafe_to_string as per manual *)
                let output = Bytes.unsafe_to_string bs in
                {c with output}

            | _ -> failwith "Not text"
                )
      | Data -> c |> output ""


let make : type a. a Decl.t -> data_t -> a cursor = 
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
    
module Type = struct
  open Decl

  type t let t: t structure = structure

  let union_tag = t |> field 0 uint16
end

  (*
   *
struct Type @0xd07378ede1f9cc60 {  # 24 bytes, 1 ptrs
  union {  # tag bits [0, 16)
    void @0 :Void;  # bits[0, 0), union tag = 0
    bool @1 :Void;  # bits[0, 0), union tag = 1
    int8 @2 :Void;  # bits[0, 0), union tag = 2
    int16 @3 :Void;  # bits[0, 0), union tag = 3
    int32 @4 :Void;  # bits[0, 0), union tag = 4
    int64 @5 :Void;  # bits[0, 0), union tag = 5
    uint8 @6 :Void;  # bits[0, 0), union tag = 6
    uint16 @7 :Void;  # bits[0, 0), union tag = 7
    uint32 @8 :Void;  # bits[0, 0), union tag = 8
    uint64 @9 :Void;  # bits[0, 0), union tag = 9
    float32 @10 :Void;  # bits[0, 0), union tag = 10
    float64 @11 :Void;  # bits[0, 0), union tag = 11
    text @12 :Void;  # bits[0, 0), union tag = 12
    data @13 :Void;  # bits[0, 0), union tag = 13
    list :group {  # union tag = 14
      elementType @14 :Type;  # ptr[0]
    }
    enum :group {  # union tag = 15
      typeId @15 :UInt64;  # bits[64, 128)
      brand @21 :Brand;  # ptr[0]
    }
    struct :group {  # union tag = 16
      typeId @16 :UInt64;  # bits[64, 128)
      brand @22 :Brand;  # ptr[0]
    }
    interface :group {  # union tag = 17
      typeId @17 :UInt64;  # bits[64, 128)
      brand @23 :Brand;  # ptr[0]
    }
    anyPointer :group {  # union tag = 18
      union {  # tag bits [64, 80)
        unconstrained :group {  # union tag = 0
          union {  # tag bits [80, 96)
            anyKind @18 :Void;  # bits[0, 0), union tag = 0
            struct @25 :Void;  # bits[0, 0), union tag = 1
            list @26 :Void;  # bits[0, 0), union tag = 2
            capability @27 :Void;  # bits[0, 0), union tag = 3
          }
        }
        parameter :group {  # union tag = 1
          scopeId @19 :UInt64;  # bits[128, 192)
          parameterIndex @20 :UInt16;  # bits[80, 96)
        }
        implicitMethodParameter :group {  # union tag = 2
          parameterIndex @24 :UInt16;  # bits[80, 96)
        }
      }
    }
  }
}
   * *)

module Brand = struct
  open Decl
  type t let t : t structure = structure

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

module Node = struct
  open Decl

  type t let t : t structure = structure

  module SourceInfo = struct 
    type t let t : t structure = structure

    let id = t |> field 0 uint64
    let docComment = t |> field 0 uint64

    module Member = struct
      type t let t : t structure = structure

      let docComment = field 0 text
    end

    let members  = field 1 @@ list Member.t
  end

  module Parameter = struct
    type t let t : t structure = structure

    let name = t |> field 0 text
    let id = t |> field 0 uint64
  end

  module NestedNode = struct
    type t let t : t structure = structure

    let name = t |> field 0 text
  end

  let id = t |> field 0 uint64
  let displayName = t |> field 0 text
  let displayNamePrefixLength = t |> field 64 uint32
  let scopeId = t |> field 128 uint64 
  let parameters = t |> field 5 @@ list Parameter.t
  let isGeneric = t |> field 288 bool
  let nestedNodes = t |> field 1 NestedNode.t
  let annotations = t |> field 2 Annotation.t
  let union_tag = t |> field 96 uint16

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
(*
 *
  capnpVersion @2 :CapnpVersion;  # ptr[2]
  nodes @0 :List(Node);  # ptr[0]
  sourceInfo @3 :List(Node.SourceInfo);  # ptr[3]
  requestedFiles @1 :List(RequestedFile);  # ptr[1]
  struct RequestedFile @0xcfea0eb02e810062 {  # 8 bytes, 2 ptrs
    id @0 :UInt64;  # bits[0, 64)
    filename @1 :Text;  # ptr[0]
    imports @2 :List(Import);  # ptr[1]
    struct Import @0xae504193122357e5 {  # 8 bytes, 1 ptrs
      id @0 :UInt64;  # bits[0, 64)
      name @1 :Text;  # ptr[0]
    }
  }
  *)

let cgr = make CodeGeneratorRequest.t a

let () =
  let open CodeGeneratorRequest in
  let open CapnpVersion in
  cgr |> get nodes |> result |> Array.iter (fun x ->
    x |> get Node.displayName |> result |> print_endline;
    x |> get Node.union_tag |> result |> Printf.printf "tag: %d\n"
  )
