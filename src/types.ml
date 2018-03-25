module Types = struct
  type 'a structure = unit

  type 'a t =
    | Void : unit t
    | Bool : bool t
    | UInt64 : Unsigned.Int64.t t
    | UInt32 : Unsigned.Int32.t t
    | UInt16 : int t
    | UInt8 : int t
    | Int64 : Int64.t t
    | Int32 : Int32.t t
    | Int16 : int t
    | Int8 : int t
    | Float32: float t
    | Float64: float t
    | Text : string t
    | Data : string t
    | List : 'a t -> 'a listproxy t
    | Struct : 'z structure * string * Unsigned.Int64.t * 'a struct_t -> ('z structure * 'a) t
    | Defer : (unit -> 'a t) -> unit t

  and 'a struct_t =
    | Cons : (string * 'a t * int * 'b struct_t) -> ('a * 'b) struct_t
    | End : unit struct_t

  and 'a listproxy = ListProxy : 'a listproxy

  let field (a, b, c) d = Cons (a, b, c, d)
  let (@>) = field;;
  let emptys = End;;

  let int16 = Int16
  let int32 = Int32

  let list x = List x
  let defer f = Defer f

  let structure = ();;

  let mkstruct t name id composite = Struct (t, name, id, composite);;
end;;

module Read = struct
  type ptr = 
    | StructPtr of int * int
    | ListPtr of int * int
    | FarPtr of int
  ;;

  let sl64 n x = Int64.shift_left x n;;
  let sl32 n x = Int32.shift_left x n;;
  let sli n x = x lsl n;;

  let read_int8 buf i = Bytes.get buf i |> Char.code;;
  let byte_f = read_int8;;

  let read_int64 buf offset =
    let f i = byte_f buf (offset + i) |> Int64.of_int |> sl64 (8 * i) in
    Int64.logor 
    (Int64.logor
    (Int64.logor (f 0) (f 1))
    (Int64.logor (f 2) (f 3)))
    (Int64.logor 
    (Int64.logor (f 4) (f 5))
    (Int64.logor (f 6) (f 7)))
  ;;

  let read_int32 buf offset =
    let f i = byte_f buf (offset + i) |> Int32.of_int |> sl32 (8 * i) in
    Int32.logor
    (Int32.logor (f 0) (f 1))
    (Int32.logor (f 2) (f 3))
  ;;

  let read_int16 buf offset =
    let f i = byte_f buf (offset + i) |> sli (8 * i) in
    (f 0) lor (f 1)
  ;;

  let rel u = Int32.shift_right_logical u 2 |> Int32.to_int;;

  let struct_ptr v = 
    Printf.printf "%ld\n" v;
    let d = Int32.logand v 0xffffl |> Int32.to_int in
    let p = Int32.shift_right_logical v 16 |> Int32.to_int in
    StructPtr (d, p)
  ;;


  let list_ptr v =
    let t = Int32.logand 3l v |> Int32.to_int in
    let n = Int32.shift_right_logical v 3 |> Int32.to_int in
    ListPtr (t, n)
  ;;

  let read_ptr (buf, offset, x) =
    let u = read_int32 buf offset in
    let v = read_int32 buf (offset + 4) in
    
    (buf, offset + 8, match (Int32.logand u 3l) with
    | 0l -> (x, (rel u, struct_ptr v))
    | 1l -> (x, (rel u, list_ptr v))
    | _ -> failwith "far pointers not implemented"
    )
  ;;

  let read_frame (buf, offset, ()) = 
    let n = read_int32 buf offset |> Int32.to_int in

    let segment_lengths = Array.make (n + 1) 0 in 

    for i = 0 to n do
      let o = offset + 4 * (i + 1) in
      segment_lengths.(i) <- Int32.to_int (read_int32 buf o);
    done;

    let offset = offset + (n + 2) * 4 in
    
    (buf, offset, segment_lengths)
  ;;


end
;;

open Types



type date

let date =
  ("year", Int16, 0) @>
  ("month", Int8, 16) @>
  ("day", Int8, 24) @>
  emptys |> mkstruct (structure : date structure) "Date" 0xef29c66fa74a8c93L;;


type phonenumber
let phonenumber = 
  ("number", Text, 0) @>
  ("type", Int16, 0) @>
  emptys |> mkstruct (structure : phonenumber structure) "PhoneNumber" 0xd68b5724fed51061L;;

type person
let rec person () =
  ("name", Text, 0) @> 
  ("email", Text, 1) @> 
  ("phones", List(phonenumber), 2) @>
  ("id", Int32, 0) @> 
  ("friends", list (defer person), 4) @>
  emptys |> mkstruct (structure : person structure) "Person" 0xa93fc509624c72d9L;;

(*

let get : type a. bytes -> a t -> a =
  fun buf addr -> 
    failwith "foo"
;;

let set : type a. bytes -> a t -> a -> unit =
  fun buf addr x -> 
    failwith "foO"
;;


let () =
  let xin = open_in "person.bin" in
  let l = in_channel_length xin in
  let buf = Bytes.create l in

  really_input xin buf 0 l;

  let buf, offset, (segment_lengths, (u, ptr)) = 
    (buf, 0, ()) |>
    read_frame |>
    read_ptr
  in
  Printf.printf "%d\n" offset;
  match ptr with
  | StructPtr (a, b) -> Printf.printf "))) %d %d\n" a b;
  ()
;;


(*
module Person = struct
  let id : 'a t -> int t = function 
    | Struct ("Person", _, _) -> failwith "wrong."
    | _ -> failwith "wrong."
end;;
  *)

(* A better interface would be: *)
(*
module Person = struct
  type t
  let name = record_field -> String t
end;;

*)
*)
