exception OrdinalError of int

module Stream = struct
  type ('data, 'a) t = { data : 'data; pos : int;  result : 'a }
  (* A very simple (read dangerous) stack based bytestream reader / writer *)
  let pop {result=(a, _); _} = a
  let popr c = {c with result=(snd c.result)}

  let map f c = {c with result = f c.result}

  let round_up d n = (((n - 1) / d) + 1) * d

  let mov d ({pos; _} as c) =  {c with pos = d + pos}
  let movw d = mov (8 * d)
  let align d c = {c with pos = round_up d c.pos}
  let push v c = {c with result = (v, c.result)}
  let pop1 ({result = (a, result); _} as c) = a, {c with result}
  let pop2 ({result = (b, (a, result)); _} as c) = (a, b), {c with result}
  let pop3 ({result = (c, (b, (a, result))); _} as z) = (a, b, c), {z with result}
  let pop4 ({result = (d, (c, (b, (a, result)))); _} as z) = (a, b, c, d), {z with result}
  let map1 f c = c |> pop1 |> (fun (a, c) -> c |> push (f a))
  let map2 f c = c |> pop2 |> (fun (a, c) -> c |> push (f a))
  let map3 f c = c |> pop3 |> (fun (a, c) -> c |> push (f a))
  let map4 f c = c |> pop4 |> (fun (a, c) -> c |> push (f a))
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
  let read_int16 c = c |> read_int8 |> read_int8 |> map2 (fun (a, b) -> a lor (b lsl 8))
  let read_int32 c = c |> read_int16 |> read_int16 |> map2 (fun (a, b) -> Int32.(logor (of_int a) (shift_left (of_int b) 16)))
  let read_int64 c = c |> read_int16 |> read_int16 |> read_int16 |> read_int16 |> map4 (fun (a, b, c, d) -> Int64.(
    logor (logor (of_int a) (shift_left (of_int b) 16)) (logor (shift_left (of_int c) 32) (shift_left (of_int d) 48) )
    ))

  let write_int8 c = c |> pop1 |> fun (x, c) -> c.data.{c.pos} <- x; c |> mov 1
  let write_int16 c = c |> pop1 |> fun (x, c) -> c |> push (x land 0xff) |> write_int8 |> push ((x lsr 8) land 255) |> write_int8
  let write_int32 c = c |> pop1 |> fun (x, c) -> 
    c |> 
    push Int32.(logand x 0xffffl |> to_int) |> write_int16  |>
    push Int32.(shift_right_logical x 16 |> to_int) |> write_int16 

  let write_int64 c = c |> pop1 |> fun (x, c) -> 
    c |> 
    push Int64.(logand x 0xffffffffL |> to_int32) |> write_int32 |>
    push Int64.(shift_right_logical x 32 |> to_int32) |> write_int32

  let write_string c = c |> pop1 |> fun (x, c) ->
    for i = 0 to (String.length x) - 1 do
      c.data.{c.pos + i} <- Char.code x.[i]
    done;
    c |> mov (String.length x)

  let withMap read f c = c |> read |> map1 f

  let write_bits64u a b c =
    let v, c = pop1 c in
    let x, c = pop1 c in
    c |> push Int64.(
      x |> of_int |> logand (sub (shift_left 1L b) 1L) |> fun x -> shift_left x a |> logor v
    )

  let read_bits64u a b c =
    let v, c = pop1 c in
    c |> push Int64.(
      logand (shift_right_logical v a) (sub (shift_left 1L b) 1L) |> to_int
      ) |> push v

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
    | Union : ('b s c -> 'a) * ('b s c builder -> 'a -> 'b s c builder) -> 'a g
    | Enum : (int -> 'a) * ('a -> int) -> 'a g
    | List : 'a g -> 'a array g
    | Ptr : 'a g -> 'a c g
    | Text : string g
    | Data : string g
    | Interface : (stored_interface array ref * Int64.t) -> 'a i g
  and stored = Stored : ('a g * 'a) -> stored
  and stored_interface = StoredInterface : 'a i g -> stored_interface
  and 'a builder = Builder of 'a * stored array

  module IntMap = Map.Make(struct type t = int let compare a b = b - a end)


  type 'a sg = 'a s c g
  type 'a ig = 'a i g
  type 'a sgu = 'a s c
  type 'a ug = 'a s g

  let sg dsize psize = Ptr (Struct (dsize, psize))
  let ig id = Interface (ref [||], id)

  type ('i, 'a, 'b) method_t = {
    method_id: int;
    method_name: string;
    iface: 'i g; (* We need to know what the interface id is when we generate calls *)
    request: 'a g;
    response: 'b g;
  }

  type ('s, 'a) field = 
    | Field of (int * int * 'a g * 'a option)
    | PtrField of (int * 'a g * 'a option)
    | Group of ('a g * 'a option)

  open Stream
  let ensure_struct_ptr c = match c.ptr with
    | StructPtr x -> x
    | _ -> failwith "Expected struct pointer"

  let ensure_compositelist_ptr c = match c.ptr with
    | CompositeListPtr (x,y) -> (x,y)
    | _ -> failwith "Expected composite list pointer"

  let ensure_list_ptr c = match c.ptr with
    | ListPtr x -> x
    | NullPtr -> {typ=0; nelem=0}
    | _ -> failwith "Expected list pointer"

  let cmap f c = {c with stream = f c.stream}

  let mov_field field c =
    let {pwords; dwords} = ensure_struct_ptr c in
    match field with
    | PtrField (n, _, _) when n >= pwords -> 
        failwith "Pointer out of range"
    | PtrField (n, t, d) ->
        c |> cmap (movw (n + dwords)) |> cmap (push (n, t, d))
    | Field (b8, _, _, _) when b8 >= (dwords * 8) -> 
        failwith "Field out of range"
    | Field (b8, n, t, d) ->
        c |> cmap (mov b8) |> cmap (push (n, t, d))
    | Group (t, d) -> c |> cmap (push (0, t, d))


  let pbuilder : type a. a s c g -> a s c builder = function
    | Ptr Struct (dwords, pwords) -> 
        let open Stream in
        let data = Array1.create int8_unsigned c_layout ((dwords + pwords) * 8) in
        let stream = {data; pos=0; result=Structured} in
        let ptrs = Array.make pwords (Stored (Void, ())) in
        Builder ({stream; ptr=StructPtr {dwords; pwords}; sections=[||]}, ptrs)
    | _ -> failwith "Builder must be given a struct."
  let openbuilder t = (Ptr t) |> pbuilder

  let write_list_ptr offset typ nelem s =
    s |> 
    push 1 |> 
    push offset |> 
    push typ |> 
    push nelem |> 
    push 0L |> 
    write_bits64u 35 29 |> 
    write_bits64u 32 3 |> 
    write_bits64u 2 30 |> 
    write_bits64u 0 2 |> 
    write_int64


  let write_struct_ptr offset dwords pwords s = 
    s |> 
    push pwords |> 
    push dwords |> 
    push offset |> 
    push 0L |>
    write_bits64u 2 30 |> 
    write_bits64u 32 16 |> 
    write_bits64u 48 16 |>
    write_int64

  type 'a stream = Stream of 'a c
  let stream_data (Stream c) = c.stream.data

  let msg : 'a c -> 'a stream = fun s ->
    let open Array1 in
    let nwords = (dim s.stream.data) / 8 in

    let data = create int8_unsigned c_layout (8 * (nwords + 2)) in
    let olddata = s.stream.data in
    let s = {s with stream = {s.stream with data; pos=0}} in
    let {pwords; dwords} = s |> ensure_struct_ptr in
    blit olddata (sub data 16 (nwords * 8));
    let s = s |> cmap (fun s -> s |> push 0l |> write_int32 |> push (Int32.of_int (nwords + 1)) |> write_int32 |> write_struct_ptr 0 dwords pwords) in
    Stream s
      
  let closebuilder : type a. a c builder -> a c = fun x ->
    (* Simply, we allocate a pool of memory of requisite size (we know this because builders track size) *)
    let Builder (s, v) = x in
    (* We first calculate the requisite size *)
    let n = v |> (0 |> Array.fold_left (fun n x -> 
      let Stored x = x in
      match x with
      | (List (Ptr _)), v ->
          (v |> (n + 8 |> Array.fold_left (fun n x -> n + (x.stream.data |> Array1.dim))))
      | Ptr _, v -> n + (v.stream.data |> Array1.dim)
      | List _, _ -> failwith "Lists not implemented yet"
      | Text, v -> n + (v |> String.length |> ((+) 1) |> round_up 8)
      | Data, _ -> failwith "Data not implemented yet"
      | Struct _, _ -> failwith "struct not implemnented yet"
      | Void, _ -> n
      | _ -> failwith "Not implemented yet!"
    )) in

    let open Bigarray in
    let m = (s.stream.data |> Array1.dim) in
    let data = Array1.create int8_unsigned c_layout (n + m) in

    Array1.(blit s.stream.data (sub data 0 m));

    let s = {s with stream={s.stream with data}} in
    let s_alloc = {s with stream={s.stream with pos=m}} in
    let s0 = {s with stream={s.stream with pos=0}} in

    v |> ((s_alloc, 0) |> Array.fold_left (fun (s, n) x -> 
      let Stored (t,v) = x in

      let s' = s0 |> mov_field (PtrField (n, t, None)) in
      let offset = (s.stream.pos - s'.stream.pos - 8) / 8 in

      (match t with
        | List Ptr (Struct (dwords, pwords)) ->
            (* We want to write the object type *)
            let nelem = Array.length v in

            let wcount = (dwords + pwords) * nelem in

            s' |> cmap (fun s -> s |> popr |> write_list_ptr offset 7 wcount) |> ignore;

            let s = s |> cmap (fun s -> s |> write_struct_ptr nelem dwords pwords) in

            v |> (s |> Array.fold_left (fun s x -> 
              let open Array1 in
              let d2_len = dim x.stream.data in
              let d2_data = sub x.stream.data 0 d2_len in
              let d1_data = sub s.stream.data s.stream.pos (dim d2_data) in
              blit d2_data d1_data;
              s |> cmap (d2_data |> dim |> mov);
            ))



        | Ptr Struct (dwords, pwords) ->
            s' |> cmap (fun s -> s |> write_struct_ptr offset dwords pwords) |> ignore;
            let open Array1 in
            let d2_len = dim v.stream.data in
            let d2_data = sub v.stream.data 0 d2_len in
            let d1_data = sub s.stream.data s.stream.pos d2_len in
            blit d2_data d1_data;
            s |> cmap (d2_data |> dim |> mov);

        | Ptr Void -> s
        | Void -> s

        | Text ->
            s' |> cmap (fun s -> s |> write_list_ptr offset 2 (1 + String.length v)) |> ignore;
            let s = s |> (cmap (fun s -> s |> push v |> write_string |> mov 1 |> align 8)) in
            s
        | _ -> failwith "match failure."

      ), (n + 1)
    )) |> fst

  let build f t = t |> openbuilder |> f |> closebuilder


  let ug f g = Group (Union (f, g), None)

  let field : type st t. st c g -> t g -> ?default:t -> Int32.t -> (st, t) field =
    fun _ t ?default offset ->
      let ptrfield = 
        let b = Int32.(to_int offset) in
        PtrField (b, t, default)
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


  let group : type st t. ?default:t -> st c g -> t g -> (st, t) field =
    fun ?default _ t ->
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
      | (offset, 7, _) ->
          c |> movw offset |> unsafe_read_struct |> map1 (fun (nelem, dwords, pwords) -> CompositeListPtr ({typ=7; nelem}, {dwords; pwords}) )
      | (offset, typ, nelem) -> 
          c |> movw offset |> push (ListPtr {typ; nelem})
    )

  let c_read_ptr c =
    let sections = c.sections in
    let rec read_ptr s = 
      match s |> read_int8 |> pop |> ((land) 3) with

      | 0 -> s |> unsafe_read_struct |> pop1 |> (function 
        | ((0, 0, 0), s) -> s |> push @@ (s.pos, NullPtr)
        | ((offset, dwords, pwords), s) -> 
          s |> movw offset |> fun s ->  s |> push @@ (s.pos, StructPtr {dwords; pwords}))
      | 1 -> s |> unsafe_read_list |> pop1 |> fun (x, s) -> s |> push (s.pos, x)
      | 2 -> s |> read_int64 |> read_bits64u 2 1 |> read_bits64u 3 29 |>
             read_bits64u 32 32  |> pop1 |> snd |> pop3 |> fun ((pad, offset, segment), s) ->
               if pad = 0 then  (
                 {s with pos = 8 * (sections.(segment) + offset)} |> read_ptr

               )
               else
                 failwith "Landing pad is two words"
      
      
      | 3 -> s |> read_int64 |> read_bits64u 2 30 |> read_bits64u 32 32 |> pop1 |> snd |> pop2 |> fst |> (
        function | (0,n) -> s |> push (s.pos, CapabilityPtr n) | _ -> failwith "match failure")
      | _ -> failwith "Impossible pointer type"

    in
    c.stream |> read_ptr |> pop1 |> fun ((_, ptr), stream) -> {c with ptr; stream}
    

  let set : type a s.  (s, a) field -> a -> s c builder -> s c builder = 
    fun field v ber ->
      let Builder (c, ptrs) = ber in
      let c0 = c in
      let c = c |> mov_field field in
      let (n, t, _), stream = c.stream |> pop1 in

      (match t with
      | UInt64 -> stream |> push v |> write_int64
      | Int64 -> stream |> push v |> write_int64
      | UInt32 -> stream |> push v |> write_int32
      | Int32 -> stream |> push v |> write_int32
      | UInt16 -> stream |> push v |> write_int16
      | Int16 -> stream |> push v |> write_int16
      | UInt8 -> stream |> push v |> write_int8
      | Int8 -> stream |> push v |> write_int8
      | List _ -> ptrs.(n) <- Stored (t, v); stream
      | Ptr _ ->
          begin match field with
          | Group _ -> Array1.blit v.stream.data c.stream.data; stream
          | _ -> ptrs.(n) <- Stored (t, v); stream end
      | Text -> ptrs.(n) <- Stored (t, v); stream
      | Union (_, g) -> g (Builder (c0 |> cmap (setval Structured), ptrs)) v |> ignore; stream
      | _ -> failwith "This is not an offset field"
      ) |> ignore;

      ber


  let get_field_type = function
      | Field (_, _, t, _) -> t
      | PtrField (_, t, _) -> t
      | Group (t, _) -> t

  let set_field_type t = function
    | Field (a, b, _, _) -> Field (a, b, t, None)
    | PtrField (a, _, _) -> PtrField (a, t, None)
    | Group (_, _) -> Group (t, None)

  (* We don't cast values, we cast addresses. *)
  let cast : type a b s. b g -> (s, a) field  -> (s, b) field =
    fun t2 field -> 
      let t1 = get_field_type field in
      match (t1, t2) with
      | (Ptr Void, List _) -> field |> set_field_type t2
      | (_, _) -> failwith "invalid cast"

  let rec show : type a. a g -> string = function
    | UInt64 -> "UInt64"
    | UInt32 -> "UInt32"
    | UInt16 -> "UInt16"
    | UInt8  -> "UInt8"
    | Int64  -> "Int64"
    | Int32  -> "Int32"
    | Int16  -> "Int16"
    | Int8   ->  "Int8"
    | Float64 -> "Float64"
    | Float32 -> "Float32"
    | Bool -> "Bool"
    | Void -> "Void"
    | Text -> "Text" 
    | Data -> "Data" 
    | Struct _ -> "Struct"
    | Ptr t -> Printf.sprintf "(Ptr %s)" (show t)
    | Union _ -> "Union"
    | List t -> Printf.sprintf "(List %s)" (show t)
    | Enum _ -> "Enum"
    | Interface _ -> "Interface"


  let get : type a s. (s, a) field -> s c -> a =
    let open Stream in
    fun field c ->
      let c = c |> mov_field field in
      
      let (b, t, default), stream = c.stream |> pop1 in

      match t with
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
        | Bool -> stream |> read_int8 |> pop |> (lsr) b |> (land) 1 |> (function | 0 -> false | _ -> true)
        | Data -> ""
        | List Ptr (Struct _) -> 
            let c = c |> c_read_ptr in
            (match c.ptr with
            | CompositeListPtr (lptr, sptr) ->
              let elem0 = {c with ptr=StructPtr sptr} |> cmap (setval Structured) in
              let swords = sptr.pwords + sptr.dwords in
              let out = Array.make lptr.nelem elem0 in
              Array.iteri (fun i _ -> 
                out.(i) <- elem0 |> cmap (movw (i * swords));
              ) out;
              out
            | NullPtr -> [| |]
            | _ -> failwith "Expected Composite List Pointer"
              )
        | Text -> 
            let c = c |> c_read_ptr in
            (match c.ptr with
            | ListPtr {nelem; typ=2} when nelem >= 1 ->
              let out = Buffer.create (nelem - 1) in
              for i = 0 to (nelem - 2) do
                Buffer.add_char out (Char.chr c.stream.data.{c.stream.pos + i});
              done;
              Buffer.contents out
            | _ -> "")


        | Ptr (Struct _) -> 
            let c = 
              (match field with | Group _ -> c | _ -> c |> c_read_ptr ) |>
              cmap (setval Structured)
            in

            c |> ensure_struct_ptr |> ignore;
            c

        | Struct _ -> 
            Structured

        | Union (f, _) -> 
            f (c |> cmap (setval Structured))

        | Ptr Void ->
            c |> cmap (setval ())

        | List Ptr _ ->
            let c = c |> c_read_ptr in
            (match c.ptr with
            | CompositeListPtr ({nelem; _}, _) when nelem >= 1 ->
                [| |]
            | ListPtr _ ->
                [| |]
            | _ -> [| |])


        | t -> failwith (Printf.sprintf "Could not match: %s" (show t))


      

  let ptr : type a. a g -> a c g = fun t -> Ptr t

  let (=>) c x =
    (get x c)

  let (>>) c x =
    (c, x)

  let (=<) = set
end

module Rpc = struct
  open Declarative

  type client
  type 'a implementation = Implemention of 'a i g

  (* A server is a collection of implementations *)
  module Int = struct
    type t = int
    let compare a b = b - a
  end

  module IntMap = Map.Make(Int)

  type imethod

  type 'a ibuilder = {
    interface: 'a;
    methods : imethod IntMap.t;
  }

  let declare : type a b. ('i g, a g, b g) method_t -> (a -> b) -> 'i ibuilder -> 'i ibuilder =
    fun _ _ _ ->
      failwith "foo"

  let implement : type a. a i g -> (a ibuilder -> a ibuilder) -> a implementation = 
    fun x _ ->
      match x with 
      | Interface _ -> failwith "foo"
      | _ -> failwith "Must be an interface"

  let connect_pipe _ =
    ()



  let call : type a b. ('i g, a g, b g) method_t -> a -> client -> b =
    fun _ _ _ ->
      failwith "connection failure"

end

module Utils = struct
  open Bigarray
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
        {stream; ptr=NullPtr; sections} |> c_read_ptr |> get (Group (t, None))

    let to_string x = 
      let open Bigarray in
      let n = Array1.dim x in
      let b = Bytes.create n in
      for i = 0 to n-1 do
        Bytes.set b i (Char.chr x.{i})
      done;
      Bytes.unsafe_to_string b

end

module type Type = sig
  open Declarative
  type t
  val t : t g
end

module type Struct = sig
  open Declarative
  type t
  val t : t s g
end

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
  module Struct(T: Struct) : Type = struct type t = T.t s c let t = Ptr T.t end
end
