
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
  methods : imethod IntMap.t Int64Map.t;
}
type 'a implementation = Implementation of 'a ibuilder 
let declare : 
  type a b. ('i, a, b) method_t -> (a -> b Lwt.t) -> 'i ibuilder -> 'i ibuilder =
    fun m f b ->
      let interface_id = match m.iface with 
      | Interface (_,_,id) -> id 
      | _ -> failwith "Expected interface." in

      let m' = IMethod(m, f) in

      let methods = b.methods |> Int64Map.update interface_id (fun x ->
        let base = match x with
        | Some x -> x
        | None -> IntMap.empty
      in
      Some (base |> IntMap.add m.method_id m'))
    in

    {b with methods}

let implement : type a. a i g -> (a ibuilder -> a ibuilder) -> a implementation = 
  fun interface f ->
    match interface with 
    | Interface _ -> Implementation ({interface; methods=Int64Map.empty} |> f)
    | _ -> failwith "Must be an interface"

let dispatch : 'a implementation -> int64 -> int -> Rpc.Message.t c -> Rpc.Message.t c Lwt.t =
  fun impl iid mid msg -> 
    impl |> ignore;
    iid |> ignore;
    mid |> ignore;
    msg |> ignore;
    failwith "not implement"




let call : type a b. ('i g, a g, b g) method_t -> a -> client -> b =
  fun _ _ _ ->
    failwith "connection failure"

