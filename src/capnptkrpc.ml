
open Capnptk.Declarative

type client

(* A server is a collection of implementations *)
module Int = struct
  type t = int
  let compare a b = b - a
end

module IntMap = Map.Make(Int)
module Int64Map = Map.Make(Int64)

type imethod = IMethod : ('i, 'a, 'b) method_t * ('a -> 'b Lwt.t) -> imethod

type 'a ibuilder = {
  interface: 'a i g;
  methods : imethod IntMap.t
}

type implementation = Implementation : 'a ibuilder  -> implementation

let declare : 
  type a b. ('i i, a, b) method_t -> (a -> b Lwt.t) -> 'i ibuilder -> 'i ibuilder =
    fun m f b ->

      let m' = IMethod(m, f) in
      let methods = b.methods |> IntMap.add m.method_id m' in
      {b with methods}

let implement : type a. a i g -> (a ibuilder -> a ibuilder) -> implementation = 
  fun interface f ->
    match interface with 
    | Interface _ -> Implementation ({interface; methods=IntMap.empty} |> f)
    | _ -> failwith "Must be an interface"


type server = implementation Int64Map.t

let interface_id = function | Interface(_, _, id) -> id | _ -> failwith "Not interface"

let server (xs : implementation array) = 
  xs |> (Int64Map.empty |> Array.fold_left (fun xs x -> 
    let Implementation b = x in
    let id = match b.interface with 
    | Interface (_, _, id) -> id
    | _ -> failwith "Not an interface." in
    Int64Map.add id x xs))


let dispatch : server -> Rpc.Call.t -> unit c Lwt.t =
  fun server call -> 
    server |> Int64Map.find (call => Rpc.Call.interfaceId) |> fun (Implementation iface) -> 
      let (IMethod (m, f)) = iface.methods |> IntMap.find (call => Rpc.Call.methodId) in
      let%lwt v = call => Rpc.Call.params => (cast_field Rpc.Payload.content m.request) |> f in
      v |> cast_struct m.response (Ptr Void) |> Lwt.return

(* Functions can be composed *)

    
let make_call : type a b. ('i, a, b) method_t -> a -> Rpc.Call.t =
  fun m x ->
    build Rpc.Call.t (fun b -> b |>
    set Rpc.Call.methodId m.method_id |>
    set Rpc.Call.interfaceId (m.iface |> interface_id) |>
    set Rpc.Call.params (build Rpc.Payload.t 
      (set (cast_field Rpc.Payload.content m.request) x)
    ))



let call : type a b. ('i g, a g, b g) method_t -> a -> client -> b Lwt.t =
  fun _ _ _ ->
    (* Basically prepare a call, send it, *)
    failwith "connection failure"

