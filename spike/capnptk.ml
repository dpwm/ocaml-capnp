module Declarative = struct
  open Bigarray
  exception Error of string

  module AnyPointer = struct 
    type t
  end


  type data = (int, int8_unsigned_elt, c_layout) Array1.t


  type 'a s = Structured
  type 'a u = Unioned

  type 'a bounds = 'a * 'a

  type 'a c = {
    data : data;
    pos: int;
    result: 'a;
  } and
  'a g =
    | Bool : bool g
    | Void : unit g
    | UInt64 : int64 g
    | UInt32 : int32 g
    | UInt16 : int g
    | UInt8 : int g

    | Int64 : int64 g
    | Int32 : int32 g
    | Int16 : int g
    | Int8 : int g

    | Float64 : float g
    | Float32 : float g

    | Struct : 'a s g
    | Union : ('b s c c -> 'a) * ('b s c c -> 'a -> unit) -> 'a g
    | List : 'a g -> 'a array g
    | Ptr : 'a g -> 'a c g
    | Text : string g
    | Data : string g
    | Interface : unit g
    | AnyPointer : unit g
  and stored = Stored : 'a g -> stored

  module Stream = struct
    (* A very simple (read dangerous) stack based bytestream reader / writer *)
    let pop {result=(a, _)} = a
    let popr c = {c with result=(fst c.result)}

    let map f c = {c with result = f c.result}


    let mov d ({pos} as c) = {c with pos = d + pos}
    let movw d = mov (8 * d)
    let align d c = {c with pos = ((((c.pos - 1) / d) + 1) * d)}
    let push v c = {c with result = (v, c.result)}
    let pop1 ({result = (a, result)} as c) = a, {c with result}
    let pop2 ({result = (b, (a, result))} as c) = (a, b), {c with result}
    let pop3 ({result = (c, (b, (a, result)))} as z) = (a, b, c), {z with result}
    let map1 f c = c |> pop1 |> (fun (a, c) -> c |> push (f a))
    let map2 f c = c |> pop2 |> (fun (a, c) -> c |> push (f a))
    let map3 f c = c |> pop3 |> (fun (a, c) -> c |> push (f a))
    let setval result c = {c with result}
    let id x = x

    let read_n read c =
      let rec f (n, c, xs) =
        match n with
        | _ when n > 0 ->
            let x, c = c |> read |> pop1 in
            f (n-1, c, x :: xs)
        | _ -> c |> push (xs |> List.rev |> Array.of_list)
      in

      c |> pop1 |> (fun (n, c) -> (n, c, [])) |> f



    let read_int8 c = c |> push c.data.{c.pos} |> mov 1
    let read_int16 c = c |> read_int8 |> read_int8 |> map2 (fun (a, b) -> a + (b lsl 8))
    let read_int32 c = c |> read_int16 |> read_int16 |> map2 (fun (a, b) -> Int32.(add (of_int a) (shift_left (of_int b) 16)))
    let read_int64 c = c |> read_int32 |> read_int32 |> map2 (fun (a, b) -> Int64.(add (of_int32 a) (shift_left (of_int32 b) 16)))

    type struct_ptr = {
      dwords : int;
      pwords : int;
    }

    type list_ptr = {
      typ: int;
      nelem : int;
    }

    type ptr =
      | StructPtr of struct_ptr
      | ListPtr of list_ptr
      | CompositeListPtr of list_ptr * struct_ptr
      | CapPtr of (int * int * int)


    let read_bits64u a b c =
      if b > 30 then failwith "read_bits64i: Maximum safe value exceeded.";
      let v, c = pop1 c in
      c |> push Int64.(logand (sub (sub (shift_left 1L (a + b)) 1L) (sub (shift_left 1L a) 1L)) v |> to_int) |> push v

    let unsigned_to_signed n x =
      let d = 1 lsl n in
      let v = (1 lsl (n - 1)) - 1 in
      if x > v then x - d else x

    let read_bits64s a b c =
      c |> read_bits64u a b |> pop2 |> fun ((x, y), c) -> c |> push (unsigned_to_signed b x) |> push y
      


    let read_header c = 
      c |>
      read_int32 |> 
      map1 (fun x -> 1 + Int32.to_int x) |> 
      read_n read_int32 |>
      align 8

    let unsafe_read_struct c = 
      c |> 
      read_int64 |>
      read_bits64s 2 30 |>
      read_bits64u 32 16 |>
      read_bits64u 48 16 |>
      pop1 |> snd |> map3 id

    let unsafe_read_list c = 
      c |>
      read_int64 |>
      read_bits64s 2 30 |>
      read_bits64u 32 3 |>
      read_bits64u 35 29 |>
      pop1 |> snd |>
      map3 id |>
      pop1 |> (fun (x, c) ->
        match x with 
        | (offset, 7, words) ->
            c |> movw offset |> unsafe_read_struct |> map1 (fun (nelem, dwords, pwords) -> CompositeListPtr ({typ=7; nelem}, {dwords; pwords}) )
        | (offset, typ, nelem) -> 
            c |> movw offset |> push (ListPtr {typ; nelem})
      )

    let read_ptr c = 
      match c |> read_int8 |> pop |> ((land) 3) with

      | 0 -> c |> unsafe_read_struct |> pop1 |> fun ((offset, dwords, pwords), c) -> 
          c |> movw offset |> fun c -> c |> push @@ (c.pos, StructPtr {dwords; pwords})

      | 1 -> c |> unsafe_read_list |> pop1 |> fun (x, c) -> c |> push (c.pos, x)
      | 2 -> failwith "not implemented"
      | 3 -> failwith "not implemented"



  end

  type 'a sg = 'a s g
  type 'a sgu = 'a s c
  type 'a ug = 'a s g

  let sg = Struct 
  let ug f g = Union (f, g)

  type ('s, 'a) field = 
    | Field of (int * int * 'a g * 'a option)
    | Group of ('a g * 'a option)


  let field : type st t. st g -> t g -> ?default:t -> Int32.t -> (st, t) field =
    fun st t ?default offset ->
      let b8 = Int32.(shift_right_logical offset 3) in
      let b = Int32.(sub offset (shift_left b8 3) |> to_int) in
      let b8 = Int32.to_int b8 in
      Field (b8, b, t, default)

  let group : type st t. ?default:t -> st g -> t g -> (st, t) field =
    fun ?default st t ->
      Group(t, default)


  let mapDefault default f x =
    match default with 
    | Some default -> f default x
    | None -> x

  let mapSome f x = match x with | Some x -> Some (f x) | None -> None


  let rec get : type a s. (s, a) field -> s c -> a =
    let open Stream in
    fun field c ->
      match field with
      | Field (b8, b, t, default) ->
        (match t with
        | UInt64 -> 
            c |> read_int64 |> pop |> mapDefault default Int64.logxor

        | Int64  ->
            c |> read_int64 |> pop |> mapDefault default Int64.logxor

        | Int32  -> 
            c |> read_int32 |> pop |> mapDefault default Int32.logxor

        | UInt32 -> 
            c |> read_int32 |> pop |> mapDefault default Int32.logxor

        | UInt16 -> 
            c |> read_int16 |> pop |> mapDefault default (lxor)

        | Int16  -> 
            c |> read_int16 |> pop |> 
            unsigned_to_signed 16 |>
            mapDefault default (lxor)

        | UInt8  -> 
            c |> read_int8 |> pop |> mapDefault default (lxor)

        | Int8   -> 
            c |> read_int8 |> pop |> unsigned_to_signed 8 |> mapDefault default (lxor)

        | Float32 -> c |> read_int32 |> pop |> mapDefault (mapSome Int32.bits_of_float default) (Int32.logxor) |> Int32.float_of_bits
        | Float64 -> c |> read_int64 |> pop |> mapDefault (mapSome Int64.bits_of_float default) (Int64.logxor) |> Int64.float_of_bits

        | Void -> ()
        | Bool -> true
        | Text -> ""
        | Data -> ""
        | List _ -> [||]
        | Ptr t -> 
            let default = match default with
            | None -> None
            | Some result -> failwith "Default values for pointer types are not implemented"
            in
            { c with result = get (Field (b8, b, t, default)) c}
        | Struct ->
            failwith "struct"
        | _ ->
            failwith "other"

        )

      | Group (t, default) -> match t with
        | Struct -> 
            Structured

        | Ptr t -> 
            let default = match default with
            | None -> None
            | Some result -> failwith "Default values for pointer types are not implemented"
            in
            { c with result = get (Group (t, default)) c}

        | _ -> failwith "A group can only result in a struct or pointer"


      

  let rec set : type a s. (s c * (s, a) field) -> a -> unit =
    fun v n ->
      ()


  let ptr : type a. a g -> a c g = fun t -> Ptr t

  let (=>) c x =
    (get x c)

  let (>>) c x =
    (c, x)

  let (=<) = set
end

module Utils = struct
  open Bigarray
  let from_stdin () =
    (* A 1MB buffer should be enough for anybody! *) 
    let max_size = (1 lsl 20) in
    let a = Array1.create int8_unsigned c_layout max_size in
    let buffer = Bytes.create 4096 in

    let rec f n = 
      let r = input stdin buffer 0 4096 in
      match r with
      | 0 -> n
      | r ->
          for i = 0 to r-1 do
            a.{n + i} <- Bytes.get buffer i |> Char.code
          done;
          f (n + r)
    in

    let n = f 0 in
    Array1.sub a 0 n

  open Declarative 
  let cursor data = 
    {data; pos=0; result=()}

  let decode : type a. a sg -> data -> a sgu = 
    fun t data ->
      let open Stream in
      cursor data |> 
      read_header |>
      read_ptr |>
      get (Group (ptr t, None))

end

