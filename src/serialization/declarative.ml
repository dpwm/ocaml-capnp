module ByteStream = Fstream.ByteStream
module Pos = Fstream.Pos

type 'a s = Structured
type 'a i = Interfaced

type ('concurrency, 'interface) ximpl_t
type ximpl = XImpl: ('concurrency, 'implementation) ximpl_t -> ximpl

type struct_ptr = {
  dwords : int;
  pwords : int;
}

type list_ptr = {
  typ: int;
  nelem : int;
}

type far_ptr = {
  double : bool;
  tpos : Pos.t;
}

type ptr =
  | NullPtr
  | StructPtr of struct_ptr
  | ListPtr of list_ptr
  | FarPtr of far_ptr
  | CompositeListPtr of list_ptr * struct_ptr
  | CapabilityPtr of int

let show_ptr = 
  let f = Printf.sprintf in
  let show_struct_ptr {dwords; pwords} = f "{dwords=%d; pwords=%d}" dwords pwords in
  let show_list_ptr {typ; nelem} = f "{typ=%d; nelem=%d}" typ nelem in
  function
  | NullPtr -> "NullPtr"
  | StructPtr x -> f "StructPtr %s" (show_struct_ptr x)
  | ListPtr x -> f "ListPtr %s" (show_list_ptr x)
  | FarPtr {double; tpos} -> f "FarPtr {double=%b; tpos=%s}" double (Pos.show tpos)
  | CompositeListPtr (x, y) -> f "CompositeListPtr (%s, %s)" (show_list_ptr x) (show_struct_ptr y)
  | CapabilityPtr x -> f "CapabilityPtr %d" x

type
  'a c = {
  stream : ByteStream.t;
  mutable ptr : ptr;
  mutable caps : (unit c) array ;
  mutable impls : int * ximpl list;
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

let c_clone c = {c with stream=ByteStream.clone c.stream}

module type Type = sig
  type t
  val t : t g
end


type 'a sg = 'a s c g
type 'a ig = 'a i c g
type 'a sgu = 'a s c
type 'a igu = 'a i c
type 'a ug = 'a s g


type ('s, 'a) field =
  | Field of (int * int * 'a g * 'a option)
  | PtrField of (int * 'a g * 'a option)
  | Group of ('a g * 'a option)

let is_ptr_typ : type a. a g -> bool = function
  | Ptr _  -> true
  | List _ -> true
  | Text  -> true
  | Data -> true
  | _ -> false

let sg dsize psize = Ptr (Struct (dsize, psize, Array.make (8 * (dsize + psize)) 0))
let ig n id = Ptr (Interface (ref [||], n, id))
let ug f g = Group (Union (f, g), None)

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

type ('i, 'a, 'b) method_t = {
  method_id: int;
  method_name: string;
  iface: 'i g; (* We need to know what the interface id is when we generate calls *)
  request: 'a g;
  response: 'b g;
}

let group : type st t. ?default:t -> st c g -> t g -> (st, t) field =
  fun ?default _ t ->
  Group(t, default)

let ensure_struct_ptr c = match c.ptr with
  | StructPtr x -> x
  | _ -> failwith "Expected struct pointer"

let mov_field field c =
  let {dwords; pwords} = ensure_struct_ptr c in
  match field with
  | PtrField (n, t, d) when n >= pwords ->
    (false, n, t, d)
  | PtrField (n, t, d) ->
    ByteStream.posmap c.stream (Pos.movw @@ n + dwords);
    (true, n, t, d)
  | Field (b8, n, t, d) when b8 >= (dwords * 8) ->
    (false, n, t, d)
  | Field (b8, n, t, d) ->
    ByteStream.posmap c.stream (Pos.mov b8);
    (true, n, t, d)
  | Group (t, d) ->
    (true, 0, t, d)


let mapDefault default f x =
  match default with
  | Some default -> f default x
  | None -> x

let mapSome f x = match x with | Some x -> Some (f x) | None -> None

let bool_of_int = function | 0 -> false | _ -> true

let read_bits_64u a b x =
  assert (b > a);
  let x = (Int64.shift_left x (64 - b)) in Int64.shift_right_logical x (64 - b + a)

let read_bits_64i a b x =
  assert (b > a);
  let x = (Int64.shift_left x (64 - b)) in Int64.shift_right x (64 - b + a)


let read_struct_ptr i =
  if read_bits_64u 0 2 i <> 0L then invalid_arg "read_struct_ptr: not a struct";
  let offs = read_bits_64i 2 32 i |> Int64.to_int in
  let dwords = read_bits_64u 32 48 i |> Int64.to_int in
  let pwords = read_bits_64u 48 64 i |> Int64.to_int in
  (offs, dwords, pwords)

let read_list_ptr i =
  if read_bits_64u 0 2 i <> 1L then invalid_arg "read_list_ptr: not a list";
  let offs = read_bits_64i 2 32 i |> Int64.to_int in
  let esize = read_bits_64u 32 35 i |> Int64.to_int in
  let lsize = read_bits_64u 35 64 i |> Int64.to_int in
  (offs, esize, lsize)

let read_cap_ptr i =
  if read_bits_64u 0 2 i <> 2L then invalid_arg "read_cap_ptr: not a capability";
  if read_bits_64u 2 32 i <> 0L then invalid_arg "read_cap_ptr: capability pointer must have zero";
  read_bits_64u 32 64 i |> Int64.to_int

let read_far_ptr i =
  if read_bits_64u 0 2 i <> 1L then invalid_arg "read_far_ptr: not a far pointer";
  let padsize = read_bits_64u 2 3 i |> Int64.to_int in
  let offs = read_bits_64u 3 32 i |> Int64.to_int in
  let seg = read_bits_64u 32 64 i |> Int64.to_int in
  (padsize=1, offs, seg)


let c_read_ptr c =
  let read_ptr c =
    let i = ByteStream.read_i64 c.stream in

    match read_bits_64u 0 2 i with
    | 0L ->
      let offs, dwords, pwords = read_struct_ptr i in
      ByteStream.posmap c.stream (Pos.movw offs);
      StructPtr {dwords; pwords}
    | 1L ->
      let offs, typ, nelem = read_list_ptr i in
      ByteStream.posmap c.stream (Pos.movw offs);
      if typ = 7 then
        let i' = ByteStream.read_i64 c.stream in
        let nelem, dwords, pwords = read_struct_ptr i' in
        CompositeListPtr ({typ; nelem}, {dwords; pwords})
      else
        ListPtr {typ; nelem}
    | 2L ->
      let double, off, seg = read_far_ptr i in
      let tpos = Pos.{seg; off} in
      FarPtr {tpos; double}
    | 3L ->
      CapabilityPtr (read_cap_ptr i)
    | _ -> failwith "invalid pointer read"

  in
  begin match read_ptr c with
    | FarPtr {double=false; tpos} ->
      ByteStream.setpos c.stream tpos;
      c.ptr <- read_ptr c
    | FarPtr {double=true; _} ->
      failwith "Double-far pointer not implemented"
    | ptr -> c.ptr <- ptr end;
  c


let rec get : type a s. (s, a) field -> s c -> a =
  fun field c ->
  ByteStream.push c.stream;
  let found, b, t, default = mov_field field c in

  if found then begin
    let (ret : a) = match t with
      | UInt64 ->
        ByteStream.read_i64 c.stream |> mapDefault default Int64.logxor
      | Int64 ->
        ByteStream.read_i64 c.stream |> mapDefault default Int64.logxor
      | Int32 ->
        ByteStream.read_i32 c.stream |> mapDefault default Int32.logxor
      | UInt32 ->
        ByteStream.read_i32 c.stream |> mapDefault default Int32.logxor
      | Int16  ->
        ByteStream.read_i16 c.stream |> mapDefault default (lxor)
      | UInt16 ->
        ByteStream.read_u16 c.stream |> mapDefault default (lxor)
      | Int8  ->
        ByteStream.read_i8 c.stream |> mapDefault default (lxor)
      | UInt8 ->
        ByteStream.read_u8 c.stream |> mapDefault default (lxor)
      | Float32 ->
        ByteStream.read_i32 c.stream |> mapDefault (mapSome Int32.bits_of_float default) (Int32.logxor) |> Int32.float_of_bits
      | Float64 ->
        ByteStream.read_i64 c.stream |> mapDefault (mapSome Int64.bits_of_float default) (Int64.logxor) |> Int64.float_of_bits
      | Void -> ()
      | Bool ->
        ByteStream.read_u8 c.stream |>
        (lsr) b |>
        (land) 1 |>
        bool_of_int |>
        begin match default with Some true -> (not) | _ -> (fun x -> x) end
      | List Ptr (Struct _) ->
        Printf.printf "Read list\n";
        let c = c_read_ptr c in
        begin match c.ptr with
          | CompositeListPtr (lptr, sptr) ->
            c.ptr <- StructPtr sptr;
            let swords = sptr.pwords + sptr.dwords in
            let elem i =
              let c = c_clone c in
              ByteStream.posmap c.stream (Pos.movw (i * swords));
              c
            in
            let out = Array.init lptr.nelem elem in
            out
          | NullPtr -> [| |]
          | _ ->
            failwith @@ "Expected Composite List Pointer or Null Pointer"
        end
      | List t ->
        let sptr = c.ptr in
        let c = c_read_ptr c in
        show_ptr c.ptr |> print_endline;
        let o = match c.ptr with
          | NullPtr -> failwith "null"
          | ListPtr {nelem; typ} when typ = 2 ->
            let s = match typ with | 2 -> 1 | 3 -> 2 | 4 -> 4 | 5 -> 8 | _ -> failwith "invalid size" in
            let c' = {c with ptr=StructPtr {pwords=0; dwords=(nelem * s + 7) / 8}}  in
            let f i =
              let f = Field (i * s, 0, t, None) in
              get f c'
            in
            Array.init nelem f
          | _ -> failwith ("ERROR: " ^ show_ptr c.ptr)
        in
        c.ptr <- sptr;
        o
      | Data -> 
        let c = c_read_ptr c in
        begin match c.ptr with
          | NullPtr -> ""
          | ListPtr {nelem; typ=2} when nelem > 0 ->
            ByteStream.read_string c.stream nelem
          | _ -> ("ERROR: " ^ show_ptr c.ptr)
        end
      | Text ->
        let c = c_read_ptr c in
        show_ptr c.ptr |> print_endline;
        begin match c.ptr with
          | NullPtr -> ""
          | ListPtr {nelem; typ=2} when nelem > 0 ->
            ByteStream.read_string c.stream (nelem - 1)
          | _ -> ("ERROR: " ^ show_ptr c.ptr)
        end
      | Ptr (Struct _) ->
        Printf.printf "Struct\n";
        let c = match field with Group _ -> c | _ -> c |> c_read_ptr in
        begin match c.ptr with
          | StructPtr _ -> ()
          | NullPtr -> ()
          | _ -> failwith "Not a struct or null ptr"
        end;
        c_clone c;
      | Ptr _ ->
        failwith "Unimplemented pointer"
      | Struct _ ->
        failwith "Cannot unpack naked struct"
      | Union _ ->
        failwith "Union unimplmented"
      | Enum _ ->
        failwith "Enum uinimplemented"
      | Interface _ ->
        failwith "Interface unimplemented"
    in
    ByteStream.pop c.stream;
    ret
  end
  else failwith "unimplemented?"


let set _ _ _ =
  failwith "Not implemented"

let (=>) c x =
  (get x c)

let (=<) x c v = set x c v

(* let cast_struct : type a b. a g -> b g -> a -> b =
   fun _t1 _t2 ->
   failwith "Not implemented"
   (* 
   match (t1, t2) with
   | (Ptr Void, Ptr Struct _) ->
    cmap (setval Structured)
   | (Ptr Struct _, Ptr Void) ->
    cmap (setval ())
   | _ -> failwith "unsupported cast"
 *)


   let cast_field : type a b. ('s, a) field -> b g -> ('s, b) field =
   fun field t ->
   match field, is_ptr_typ t with
   | PtrField (i, _, _), true -> PtrField (i, t, None)
   | _ -> failwith "cannot cast data or group field."

*)
