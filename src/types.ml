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
  | Struct : string * Unsigned.Int64.t * 'a struct_t -> 'a t
  | Defer : (unit -> 'a t) -> unit t

and 'a struct_t =
  | Cons : (string * 'a t * int * 'b struct_t) -> ('a * 'b) struct_t
  | End : unit struct_t

and 'a listproxy = ListProxy : 'a listproxy

let field (a, b, c) d = Cons (a, b, c, d)
let (@>) = field;;
let emptys = End;;

let mkstruct name id composite = Struct (name, id, composite);;

let date =
  ("year", Int16, 0) @>
  ("month", Int8, 16) @>
  ("day", Int8, 24) @>
  emptys |> mkstruct "Date" 0xef29c66fa74a8c93L;;

let phonenumber = 
  ("number", Text, 0) @>
  ("type", Int16, 0) @>
  emptys |> mkstruct "PhoneNumber" 0xd68b5724fed51061L;;

let rec person () =
  ("name", Text, 0) @> 
  ("email", Text, 1) @> 
  ("phones", List(phonenumber), 2) @>
  ("id", Int32, 0) @> 
  ("friends", List(Defer(fun () -> person ())), 4) @>
  emptys |> mkstruct "Person" 0xa93fc509624c72d9L;;

let get : type a. bytes -> a t -> a =
  fun buf addr -> 
    failwith "foo"
;;

let set : type a. bytes -> a t -> a -> unit =
  fun buf addr x -> 
    failwith "foO"
;;
