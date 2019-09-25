open Bigarray

type data = (int, int8_unsigned_elt, c_layout) Array1.t

type 'a s = Structured
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

type ('concurrency, 'interface) ximpl_t
type ximpl = XImpl: ('concurrency, 'implementation) ximpl_t -> ximpl

let empty_impls = (0, [])


type
  'a c = {
  stream : (data, 'a) Stream.t;
  ptr : ptr;
  caps : (unit c) array ;
  impls : int * ximpl list;
  sections : int array;
} and
  'a g =
    | Bool : bool g
    | Void : unit g

    (* Unsigned integers are just same as signed integers. You only have to be
      * careful with division and printing. *)
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
    (* Float32 is conveniently a subset of Float64 *)

    | Struct : int * int * int array -> 'a s g
    (* A struct is data words, pointer words, and a mask. *)

    | Union : ('b s c -> 'a) * ('b s c builder -> 'a -> 'b s c builder) -> 'a g
    (* A union is a mapping *)

    | Enum : (int -> 'a) * ('a -> int) -> 'a g
    | List : 'a g -> 'a array g
    | Ptr : 'a g -> 'a c g
    | Text : string g
    | Data : string g
    | Interface : (stored_interface array ref * int * Int64.t) -> 'a i g
    (* An interface stores the superclasses, the number of methods and the id *)

and stored_interface = StoredInterface : 'a i g -> stored_interface
(* This is needed for Interface *)
and 'a builder = Builder of 'a * stored array
and stored = Stored : ('a g * 'a) -> stored
(* Sometimes we wish to store things. This is a simple existential type allowing a stored value with its type. *)

type ('i, 'a, 'b) method_t = {
  method_id: int;
  method_name: string;
  iface: 'i g; (* We need to know what the interface id is when we generate calls *)
  request: 'a g;
  response: 'b g;
}

let null_data = Array1.create int8_unsigned c_layout 0

let cap_ptr cap =
  let data = null_data in
  let open Stream in
  {stream = {data; pos=0; result=Interfaced}; impls=(1, [cap]); ptr=CapabilityPtr 0;
   caps=[||]; sections=[|0|]}

(* It's not enough to represent a method. We also need to represent a method
  * containing an Lwt thread. This is somewhat troublesome as we cannot know
  * anything about Lwt in here and we don't wish to functorize for the following reasons:
    * Any functor must be instatiated in. This is offputting to those unfamiliar with the module language.
    * Any imported modules must also be functorized appropriately.
    *
    * We use functors for other things
    *
    * *)


module IntMap = Map.Make(struct type t = int let compare a b = b - a end)




type 'a sg = 'a s c g
type 'a ig = 'a i c g
type 'a sgu = 'a s c
type 'a igu = 'a i c
type 'a ug = 'a s g

let sg dsize psize = Ptr (Struct (dsize, psize, Array.make (8 * (dsize + psize)) 0))
let ig n id = Ptr (Interface (ref [||], n, id))

(* I don't think we actually need to have the interface be aware of the
  * methods it holds. By making the default implementation an error we don't
  * need to know. *)

(* It would be great if we could add an  *)
let defmethod : type t a b. t i c g -> a s c g -> b s c g -> int -> string -> (t i c, a s c, b s c) method_t =
  fun iface request response method_id method_name -> 
  let m = { method_id; method_name;  request; response;  iface } in
  m


module type Type = sig
  type t
  val t : t g
end

type ('s, 'a) field =
  | Field of (int * int * 'a g * 'a option)
  | PtrField of (int * 'a g * 'a option)
  | Group of ('a g * 'a option)

open Stream
let ensure_struct_ptr c = match c.ptr with
  | StructPtr x -> x
  | _ -> failwith "Expected struct pointer"

let ensure_cap_ptr c = match c.ptr with
  | CapabilityPtr n -> n
  | _ -> failwith "Expected capability pointer"

let ensure_compositelist_ptr c = match c.ptr with
  | CompositeListPtr (x,y) -> (x,y)
  | _ -> failwith "Expected composite list pointer"

let ensure_list_ptr c = match c.ptr with
  | ListPtr x -> x
  | NullPtr -> {typ=0; nelem=0}
  | _ -> failwith "Expected list pointer"

let cmap f c = {c with stream = f c.stream}

let c_nulldata = Array1.create int8_unsigned c_layout 8
let c_nullptr c = {stream = {data = c_nulldata; pos=0; result=c.stream.result}; ptr=NullPtr; caps=[||]; impls=(0, []); sections=[|0|]}

let mov_field field c =
  let {pwords; dwords} = ensure_struct_ptr c in
  match field with
  | PtrField (n, t, d) when n >= pwords -> 
    c_nullptr c |> cmap (push (n, t, d))
  | PtrField (n, t, d) ->
    c |> cmap (movw (n + dwords)) |> cmap (push (n, t, d))
  | Field (b8, _, _, _) when b8 >= (dwords * 8) -> 
    failwith "Field out of range"
  | Field (b8, n, t, d) ->
    c |> cmap (mov b8) |> cmap (push (n, t, d))
  | Group (t, d) -> c |> cmap (push (0, t, d))


let openbuilder : type a. a s c g -> a s c builder = function
  | Ptr Struct (dwords, pwords, _) -> 
    let open Stream in
    let data = Array1.create int8_unsigned c_layout ((dwords + pwords) * 8) in
    Array1.fill data 0;
    let stream = {data; pos=0; result=Structured} in
    let ptrs = Array.make pwords (Stored (Void, ())) in
    Builder ({stream; ptr=StructPtr {dwords; pwords}; impls=empty_impls; caps=[||]; sections=[||]}, ptrs)
  | _ -> failwith "Builder must be given a struct."

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

let write_cap_ptr n s = 
  s |> 
  push n |>
  push 0 |>
  push 3 |> 
  push 0L |>
  write_bits64u 0 2 |>
  write_bits64u 2 30 |> 
  write_bits64u 32 32 |> 
  write_int64


let write_ptr ptr s1 s2 =
  let offset = ((s2.pos - s1.pos) / 8) - 1 in
  match ptr with
  | NullPtr -> (s1 |> push 0L |> write_int64), s2
  | StructPtr {dwords; pwords} -> (write_struct_ptr offset dwords pwords s1), s2
  | ListPtr {typ; nelem} -> (write_list_ptr offset typ nelem s1), s2
  | CompositeListPtr ({typ; nelem}, {dwords; pwords}) ->
    let s1 = write_list_ptr offset typ (nelem * (dwords + pwords)) s1 in
    let s2 = write_struct_ptr nelem dwords pwords s2 in
    s1, s2
  | CapabilityPtr n  -> (write_cap_ptr n s1), s2 

type 'a stream = Stream of 'a c
let msg_data (Stream c) = c.stream.data

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

let size_of : type a. a g -> int =
  (* Returns the size of the base type, in bits. *)
  fun t ->
  match t with
  | Void -> 0
  | Bool -> 1
  | Int8 -> 8
  | UInt8 -> 8
  | Int16 -> 16
  | UInt16 -> 16
  | Enum _ -> 16
  | Int32 -> 32
  | UInt32 -> 32
  | Float32 -> 32
  | Int64 -> 64
  | UInt64 -> 64
  | Float64 -> 64
  | _ -> failwith "size_of: this type has runtime-dependent size."

let closebuilder : type a. a c builder -> a c = fun x ->
  (* Simply, we allocate a pool of memory of requisite size (we know this
     because builders track size) *)
  let Builder (s, v) = x in
  (* We first calculate the requisite size *)
  let n = v |> (0 |> Array.fold_left (fun n x -> 
      let Stored x = x in
      match x with
      | (List (Ptr _)), v ->
        (v |> (n + 8 |> Array.fold_left (fun n x -> n + (x.stream.data |> Array1.dim))))
      (* This is the same as a *)
      | List Text, v ->
        v |> Array.fold_left (fun n x ->
            n + 9 + String.length x) 0
      | List Data, v ->
        v |> Array.fold_left (fun n x ->
            n + 8 + String.length x) 0
      | Ptr _, v -> n + (v.stream.data |> Array1.dim)
      | List List _, _ -> failwith "List of list not implemented"
      | List t, v -> t |> size_of |> fun x -> x * (Array.length v) / 8 |> round_up 8
      | Text, v -> n + (v |> String.length |> ((+) 1) |> round_up 8)
      | Data, v -> n + (v |> String.length |> round_up 8)
      | Struct _, _ -> failwith "struct not implemnented yet"
      | Void, _ -> n
      | _ -> failwith "Not implemented yet!"
    )) in

  let open Bigarray in
  let m = (s.stream.data |> Array1.dim) in
  let data = Array1.create int8_unsigned c_layout (n + m) in
  Array1.fill data 0;

  Array1.(blit s.stream.data (sub data 0 m));

  let s = {s with stream={s.stream with data}} in
  let s_alloc = {s with stream={s.stream with pos=m}} in
  let s0 = {s with stream={s.stream with pos=0}} in

  v |> ((s_alloc, 0) |> Array.fold_left (fun (s, n) x -> 
      let Stored (t,v) = x in

      let s' = s0 |> mov_field (PtrField (n, t, None)) |> cmap popr in
      let offset = (s.stream.pos - s'.stream.pos - 8) / 8 in

      (match t with
       | List Ptr (Struct (dwords, pwords, _)) ->
         (* We want to write the object type *)
         let nelem = Array.length v in

         let wcount = (dwords + pwords) * nelem in

         s' |> cmap (write_list_ptr offset 7 wcount) |> ignore;

         let s = s |> cmap (fun s -> s |> write_struct_ptr nelem dwords pwords) in

         v |> (s |> Array.fold_left (fun s x -> 
             let open Array1 in
             let d2_len = dim x.stream.data in
             let d2_data = sub x.stream.data 0 d2_len in
             let d1_data = sub s.stream.data s.stream.pos (dim d2_data) in
             blit d2_data d1_data;
             s |> cmap (d2_data |> dim |> mov);
           ))

       | List Text ->
         let nelem = Array.length v in

         s' |> cmap (write_list_ptr offset 6 nelem) |> ignore;

         (* Mov by nelem words *)

         let ps = s in
         let ds = s |> (cmap (movw nelem)) in

         v |> ((ps, ds) |> Array.fold_left (fun (ps, ds) v ->
             let o = ((ds.stream.pos - ps.stream.pos) / 8) - 1 in
             let ps = ps |> cmap (write_list_ptr o 2 (1 + String.length v)) in
             let ds = ds |> cmap (fun s -> s |> push v |> write_string |> mov 1 |> align 8) in
             (ps, ds)
           )) |> snd

       | List Data ->
         let nelem = Array.length v in

         s' |> cmap (write_list_ptr offset 6 nelem) |> ignore;

         (* Mov by nelem words *)

         let ps = s in
         let ds = s |> (cmap (movw nelem)) in

         v |> ((ps, ds) |> Array.fold_left (fun (ps, ds) v ->
             let o = ((ds.stream.pos - ps.stream.pos) / 8) - 1 in
             let ps = ps |> cmap (write_list_ptr o 2 (String.length v)) in
             let ds = ds |> cmap (fun s -> s |> push v |> write_string |> align 8) in
             (ps, ds)
           )) |> snd


       | List Bool ->
         let nelem = Array.length v in
         let wsize = round_up 8 nelem in

         s' |> cmap (write_list_ptr offset 1 nelem) |> ignore;
         s |> cmap (movw wsize)

       | List t ->
         let nelem = Array.length v in
         let size = size_of t in

         let elsize = match size with
           | 64 -> 5
           | 32 -> 4
           | 16 -> 3
           | 8 -> 2
           | 1 -> 1
           | _ -> 0
         in
         elsize |> ignore;

         s' |> cmap (write_list_ptr offset elsize nelem) |> ignore;



         (* Mov by nelem words *)


         s


       | Ptr Struct (dwords, pwords, _) ->
         s' |> cmap (fun s -> s |> write_struct_ptr offset dwords pwords) |> ignore;
         let open Array1 in
         let d2_len = dim v.stream.data in
         let d2_data = sub v.stream.data 0 d2_len in
         let d1_data = sub s.stream.data s.stream.pos d2_len in
         blit d2_data d1_data;
         s |> cmap (d2_data |> dim |> mov);

       | Ptr Interface _ -> 
         let m = ensure_cap_ptr v in
         s' |> cmap (fun s -> s |> write_cap_ptr m) |> ignore;
         {s with impls = v.impls}

       | Ptr Void -> 
         let open Array1 in
         let _, stream = write_ptr v.ptr s'.stream s.stream in 
         let d2_len = dim v.stream.data in
         let d2_data = sub v.stream.data 0 d2_len in
         let d1_data = sub stream.data stream.pos (dim d2_data) in
         blit d2_data d1_data;
         {s with stream} |> cmap (d2_data |> dim |> mov)
       | Void -> 
         s

       | Text ->
         s' |> cmap (fun s -> s |> write_list_ptr offset 2 (1 + String.length v)) |> ignore;
         let s = s |> (cmap (fun s -> s |> push v |> write_string |> mov 1 |> align 8)) in
         s
       | Data ->
         s' |> cmap (fun s -> s |> write_list_ptr offset 2 (String.length v)) |> ignore;
         let s = s |> (cmap (fun s -> s |> push v |> write_string |> align 8)) in
         s
       | _ -> failwith "match failure."

      ), (n + 1)
    )) |> fst |> cmap (setpos 0)


let copy_impls (Builder (c1, _)) (Builder(c2,ptrs)) =
  Builder ({c2 with impls = c1.impls}, ptrs)



let build t f = 
  t |> openbuilder |> f |> closebuilder


let ug f g = Group (Union (f, g), None)

let is_ptr_typ : type a. a g -> bool = function
  | Ptr _  -> true
  | List _ -> true
  | Text  -> true
  | Data -> true
  | _ -> false



let field : type st t. st c g -> t g -> ?default:t -> Int32.t -> (st, t) field =
  fun _ t ?default offset ->
  let ptrfield = 
    let b = Int32.(to_int offset) in
    PtrField (b, t, default)
  in
  match is_ptr_typ t with 
  | true -> ptrfield
  | false -> 
    let b8 = Int32.(shift_right_logical offset 3) in
    let b = Int32.(logand b8 7l |> to_int ) in
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

let get_interface_capability : _ i c -> int32 = 
  fun c ->
  match (c |> c_read_ptr).ptr with
  | CapabilityPtr x -> 
    x |> Int32.of_int
  | _ -> failwith "Not a capability pointer."

let add_impl x (n, xs) =
  (n + 1, x :: xs)

let set : type a s.  (s, a) field -> a -> s c builder -> s c builder = 
  fun field v ber ->
  let Builder (c, ptrs) = ber in
  let c0 = c in
  let c = c |> mov_field field in
  let (n, t, _), stream = c.stream |> pop1 in

  let ber = ref ber in

  (* we need to fix this! *)
  (match t with
   | Bool -> stream
   | UInt64 -> stream |> push v |> write_int64 (* NO! *)
   | Int64 -> stream |> push v |> write_int64
   | UInt32 -> stream |> push v |> write_int32
   | Int32 -> stream |> push v |> write_int32
   | UInt16 -> stream |> push v |> write_int16
   | Int16 -> stream |> push v |> write_int16
   | UInt8 -> stream |> push v |> write_int8
   | Int8 -> stream |> push v |> write_int8
   | Enum (_, g) -> stream |> push (g v) |> write_int16
   | List _ -> ptrs.(n) <- Stored (t, v); stream
   | Ptr Void -> ptrs.(n) <- Stored (t, v); stream
   | Ptr (Struct _) ->
     (* These need to be prepended. Don't worry, for now, about existing caps. *)
     begin match field with
       | Group _ ->
         (* There is a prblem here. *)
         (* We could have pointers and they will cause varying length.*)
         (* Is it time to deal with this completely? *)
         Array1.blit v.stream.data c.stream.data; stream
       | _ -> ptrs.(n) <- Stored (t, v); stream end
   | Ptr (Interface _) ->
     (* 
          * If you encounter an interface, it is going to have just at most
          * one implementation and its pointer will be zero.
          * It is not currently possible to implement an unimplmented capability.
          * However, structures may have multiple capabilities, and we should advance accordingly.
          * It's hard to implement something you know is crap.
          * Nontheless, we will.
          * 
          * *)
     let m = fst c.impls in
     let impls = c.impls |> (v.impls |> snd |> List.hd |> add_impl) in
     ptrs.(n) <- Stored (t, {v with ptr = CapabilityPtr m});
     let c = {c0 with impls} in
     ber := Builder (c, ptrs); stream
   | Text -> ptrs.(n) <- Stored (t, v); stream
   | Data -> ptrs.(n) <- Stored (t, v); stream
   | Union (_, g) -> g (Builder (c0 |> cmap (setval Structured), ptrs)) v |> ignore; stream
   | _ -> failwith "Cannot set type"
  ) |> ignore;

  !ber

let get_field_type = function
  | Field (_, _, t, _) -> t
  | PtrField (_, t, _) -> t
  | Group (t, _) -> t


let setb : type a t. (t, a s c) field -> 
  (a s c builder -> a s c builder) -> t c builder -> t c builder =
  fun field f ber ->
  (* We just allocate in a zero-copy kind of way. *)
  let ber2 = openbuilder (get_field_type field) |> copy_impls ber |> f in
  let v = (ber2 |> closebuilder) in
  set field v ber |> copy_impls ber2


(* Recall that a cast can only take place between pointer types. List is complicated, but do-able. *)
(* This is problematic. Really we need to think about this. *)
(* In order to turn an anyptr into a list, we need to *)
(* It might be easier to cast the references, given everything must be in a struct anyway. *)

let cast_field : type a b. ('s, a) field -> b g -> ('s, b) field =
  fun field t ->
  match field, is_ptr_typ t with
  | PtrField (i, _, _), true -> PtrField (i, t, None)
  | _ -> failwith "cannot cast data or group field."

let to_void : 'a c -> unit c = fun x -> cmap (setval ()) x

let cast_struct : type a b. a g -> b g -> a -> b =
  fun t1 t2 -> 
  match (t1, t2) with
  | (Ptr Void, Ptr Struct _) -> 
    cmap (setval Structured)
  | (Ptr Struct _, Ptr Void) -> 
    cmap (setval ())
  | _ -> failwith "unsupported cast"




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
    | Bool -> stream |> read_int8 |> pop |> (lsr) b |> (land) 1 |> (function | 0 -> false | _ -> true) |> (match default with Some true -> (not) | _ -> (fun x -> x))
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

      (match c.ptr with
       | StructPtr _  -> ()
       | NullPtr -> ()
       | _ -> failwith "Not a struct or null ptr");
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

    | Ptr (Interface _) ->
      c |> cmap (setval (Interfaced))

    | t -> failwith (Printf.sprintf "Could not match: %s" (show t))


let void_get_ptr : int -> unit c -> unit c =
  fun n c ->
  c |> mov_field (PtrField (n, (Ptr Void), None)) |> cmap popr |> c_read_ptr



let ptr : type a. a g -> a c g = fun t -> Ptr t

let (=>) c x =
  (get x c)

let (>>) c x =
  (c, x)

let (=<) = set
