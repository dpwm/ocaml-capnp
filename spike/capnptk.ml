
module Stream = struct
  type ('data, 'a) t = { data : 'data; pos : int;  result : 'a }
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
  let read_int64 c = c |> read_int32 |> read_int32 |> map2 (fun (a, b) -> Int64.(add (of_int32 a) (shift_left (of_int32 b) 32)))

  let withMap read f c = c |> read |> map1 f



  let read_bits64u a b c =
    let v, c = pop1 c in
    c |> push Int64.(logand (shift_right_logical v a) (sub (shift_left 1L b) 1L) |> to_int) |> push v

  let unsigned_to_signed n x =
    let d = 1 lsl n in
    let v = (1 lsl (n - 1)) - 1 in
    if x > v then x - d else x

  let read_bits64s a b c =
    c |> read_bits64u a b |> pop2 |> fun ((x, y), c) -> c |> push (unsigned_to_signed b x) |> push y
    





end
module Declarative = struct
  open Bigarray
  exception Error of string

  type data = (int, int8_unsigned_elt, c_layout) Array1.t


  type 'a s = Structured
  type 'a u = Unioned
  type 'a i = Interfaced

  type 'a bounds = 'a * 'a

  type struct_ptr = {
    dwords : int;
    pwords : int;
  }

  type list_ptr = {
    typ: int;
    nelem : int;
  }

  type ptr =
    | NullPtr 
    | StructPtr of struct_ptr
    | ListPtr of list_ptr
    | CompositeListPtr of list_ptr * struct_ptr
    | CapabilityPtr of int

  type 'a c = {
    stream : (data, 'a) Stream.t;
    ptr : ptr;
    sections : int array;
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

    | Struct : int * int -> 'a s g
    | Union : ('b s c -> 'a) * ('b s c -> 'a -> unit) -> 'a g
    | Enum : ('b -> 'a) * ('a -> 'b) -> 'a g
    | List : 'a g -> 'a array g
    | Ptr : 'a g -> 'a c g
    | Text : string g
    | Data : string g
    | Interface : 'a i g
    | Method : 'a g * 'b g -> ('a -> 'b) g
  and stored = Stored : 'a g -> stored

  module IntMap = Map.Make(struct type t = int let compare a b = b - a end)

  type 'a builder = Builder of 'a * stored IntMap.t

  type 'a sg = 'a s g
  type 'a ig = 'a i g
  type 'a sgu = 'a s c
  type 'a ug = 'a s g


  let builder : type a. a g -> a builder = function
    | Ptr (Struct (dwords, pwords)) -> 
        let open Stream in
        let data = Array1.create int8_unsigned c_layout (1 + dwords + pwords) in
        let stream = {data; pos=0; result=Structured} in
        Builder ({stream; ptr=StructPtr {dwords; pwords}; sections=[||]}, IntMap.empty)



  let sg dsize psize = Struct (dsize, psize)
  let ig = Interface

  type ('s, 'a) field = 
    | Field of (int * int * 'a g * 'a option)
    | Group of ('a g * 'a option)

  let ug f g = Group (Union (f, g), None)


  let field : type st t. st g -> t g -> ?default:t -> Int32.t -> (st, t) field =
    fun st t ?default offset ->
      let ptrfield = 
        let b8 = Int32.(to_int offset) in
        let b = 0 in
        Field (b8, b, t, default)
      in
      match t with 
      | Ptr _  -> ptrfield
      | List _ -> ptrfield
      | Text  -> ptrfield
      | Data -> ptrfield


      | _ -> 
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

  open Stream
  let read_header c = 
    c |>
    read_int32 |> 
    map1 (fun x -> 1 + Int32.to_int x) |> 
    read_n (withMap read_int32 Int32.to_int) |>
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


  let c_read_ptr c =
    let sections = c.sections in
    let rec read_ptr s = 
      match s |> read_int8 |> pop |> ((land) 3) with

      | 0 -> s |> unsafe_read_struct |> pop1 |> fun ((offset, dwords, pwords), s) -> 
          s |> movw offset |> fun s -> s |> push @@ (s.pos, StructPtr {dwords; pwords})

      | 1 -> s |> unsafe_read_list |> pop1 |> fun (x, s) -> s |> push (s.pos, x)
      | 2 -> s |> read_int64 |> read_bits64u 2 1 |> read_bits64u 3 29 |>
             read_bits64u 32 32  |> pop1 |> snd |> pop3 |> fun ((pad, offset, segment), s) ->
               if pad = 0 then  (
                 {s with pos = 8 * (sections.(segment) + offset)} |> read_ptr

               )
               else
                 failwith "Landing pad is two words"
      
      | 3 -> s |> read_int64 |> read_bits64u 2 30 |> read_bits64u 32 32 |> pop1 |> snd |> pop2 |> fst |> function | (0,n) -> s |> push (s.pos, CapabilityPtr n)

    in
    c.stream |> read_ptr |> pop1 |> fun ((_, ptr), stream) -> {c with ptr; stream}
    
  let ensure_struct_ptr c = match c.ptr with
    | StructPtr x -> x
    | _ -> failwith "Expected struct pointer"

  let ensure_compositelist_ptr c = match c.ptr with
    | CompositeListPtr (x,y) -> (x,y)
    | _ -> failwith "Expected composite list pointer"

  let ensure_list_ptr c = match c.ptr with
    | ListPtr x -> x
    | _ -> failwith "Expected list pointer"

  let cmap f c = {c with stream = f c.stream}
  let mov_field : type a. a g -> int -> 'b c -> 'b c = fun t b8 c  ->
    let {pwords; dwords} = ensure_struct_ptr c in
    let p () =
      if b8 >= pwords then failwith "Pointer out of bounds";
      cmap (movw (b8 + dwords)) c
    in
    match t with
    | Ptr _ -> p ()
    | List _ ->  p ()
    | Text ->  p ()
    | Data -> p ()
    | _ -> 
        if b8 >= (dwords * 8) then failwith (Printf.sprintf "Data out of bounds");
        cmap (mov b8) c



  let get : type a s. (s, a) field -> s c -> a =
    let open Stream in
    fun field c ->
      match field with
      | Field (b8, b, t, default) ->
        (* We also must make sure that we are in a struct pointer *)

        let c = c |> mov_field t b8 in
        let stream = c.stream in
        (match t with
        | UInt64 -> 
            stream |> read_int64 |> pop |> mapDefault default Int64.logxor

        | Int64  ->
            stream |> read_int64 |> pop |> mapDefault default Int64.logxor

        | Int32  -> 
            stream |> read_int32 |> pop |> mapDefault default Int32.logxor

        | UInt32 -> 
            stream |> read_int32 |> pop |> mapDefault default Int32.logxor

        | UInt16 -> 
            stream |> read_int16 |> pop |> mapDefault default (lxor)

        | Int16  -> 
            stream |> read_int16 |> pop |> 
            unsigned_to_signed 16 |>
            mapDefault default (lxor)

        | UInt8  -> 
            stream |> read_int8 |> pop |> mapDefault default (lxor)

        | Int8   -> 
            stream  |> read_int8 |> pop |> unsigned_to_signed 8 |> mapDefault default (lxor)

        | Float32 -> stream |> read_int32 |> pop |> mapDefault (mapSome Int32.bits_of_float default) (Int32.logxor) |> Int32.float_of_bits
        | Float64 -> stream |> read_int64 |> pop |> mapDefault (mapSome Int64.bits_of_float default) (Int64.logxor) |> Int64.float_of_bits

        | Void -> ()
        | Bool -> true
        | Data -> ""
        | List Ptr (Struct _)-> 
            let c = c |> c_read_ptr in
            let lptr, sptr = c |> ensure_compositelist_ptr in
            Printf.printf "%d %d %d\n" sptr.dwords sptr.pwords lptr.nelem;
            let elem0 = {c with ptr=StructPtr sptr} |> cmap (setval Structured) in
            let swords = sptr.pwords + sptr.dwords in
            let out = Array.make lptr.nelem elem0 in
            Array.iteri (fun i _ -> 
              out.(i) <- elem0 |> cmap (movw (i * swords));
            ) out;
            out

        | Text -> 
            let c = c |> c_read_ptr in
            let v = c |> ensure_list_ptr in
            if v.typ <> 2 then failwith "Expected byte list, got something else";
            let out = Buffer.create (v.nelem - 1) in
            for i = 0 to (v.nelem - 2) do
              Buffer.add_char out (Char.chr c.stream.data.{c.stream.pos + i});
            done;
            Buffer.contents out


        | Ptr (Struct _) -> 
            let default = match default with
            | None -> None
            | Some result -> failwith "Default values for pointer types are not implemented"
            in
            let c = c |> c_read_ptr |> cmap (setval Structured) in
            c |> ensure_struct_ptr;
            c

        | Struct _ ->
            failwith "struct"
        | _ ->
            failwith "other"

        )

      | Group (t, default) -> match t with
        | Struct _ -> 
            Structured

        | Ptr (Struct _) -> 
            let default = match default with
            | None -> None
            | Some result -> failwith "Default values for pointer types are not implemented"
            in
            (match c.ptr with
            | StructPtr _ -> {c with stream = c.stream |> setval Structured}
            | _ -> failwith "Expected Struct Pointer, got something else")

        | Union (f, g) -> 
            f (c |> cmap (setval Structured))


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
  open Declarative
  open Stream
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
      pop1 |> fun (sections, stream) -> 
        let pos = ref (stream.pos / 8) in
        let sections = sections |> Array.map (fun x -> let v = !pos in pos := !pos + x; v) in
        Array.iter (fun x -> Printf.printf ":%d\n" x) sections;
        {stream; ptr=NullPtr; sections} |> c_read_ptr |> get (Group (ptr t, None))

end

