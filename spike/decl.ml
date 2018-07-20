type 'a struct_data = StructData

type data_t = 
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a cursor = {
  pos : int;
  data : data_t;
  output: 'a;
  dwords: int;
  pwords: int;
  frames: int array;
}

let sptr = [2; 30; 16; 16]
let lptr = [2; 30; 3; 29]
let fptr = [2; 1; 29; 32]
let cptr = [2; 30; 32]

let split ns x =
  let rec f x = function
    | (n :: ns) ->
        Int64.((shift_left 1L n |> (add (-1L)) |> logand x) :: f (shift_right_logical x n) ns)
    | [] -> []
  in
  f x ns

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
type ('t, 'a) group = Group of 'a t

type stored = Stored : 'a t -> stored

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

let group : type a. a t -> 't t -> ('t, a) group =
  fun t2 t1 -> Group t2

let getGroup : type a. ('b, a) group -> 'b cursor -> a cursor =
  fun (Group t) c ->
    match t with
    | Struct -> c |> output StructData
    | _ -> failwith "Can only get group from struct"

let get : type a. ('b, a) field -> 'b cursor -> a cursor =
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
            | [0; 0; 0; 0] -> {c with output=[| |]}
            | _ -> failwith "not implemented yet";

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

module Infix = struct
  let (=>*) a b = a |> get b |> result
  let (=>) a b = a |> get b 
end
