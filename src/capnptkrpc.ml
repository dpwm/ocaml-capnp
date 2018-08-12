open Capnptk.Declarative

(* A server is a collection of implementations *)
module Int = struct
  type t = int
  let compare a b = b - a
end

module IntMap = Map.Make(Int)
module Int32Map = Map.Make(Int32)
module Int64Map = Map.Make(Int64)

type client

module Uint8Array1 = struct
  open Bigarray

  type t = (int, int8_unsigned_elt, c_layout) Array1.t

  external of_bytes : Lwt_bytes.t -> t = "%identity"
  external to_bytes : t -> Lwt_bytes.t = "%identity"
end


(* The implementation table is *)
(* Basically, an implementation can just be a pointer into the implementation
 * table. When an export is made it is linked to the export table. *)

(* So when a struct does the returning thing, it can be sure that there will be
 * a suitable export. *)

(* We **could** always require capabilities (implementations) to be defined
 * SEPARATELY. However, this would be a bit of a pain. 
 *
 * The captable is not sufficient for this. 
 *
 * Instead we need a space for an implementation table to go. This is
 * applicable to both "client" and "server" as both can actually hold this.
 *
 * *)





type imethod = IMethod : ('i, 'a, 'b) method_t * ('a -> 'b Lwt.t) -> imethod

type 'a ibuilder = {
  interface: 'a i g;
  methods : method_m option array
}

type implementation = Implementation : 'a ibuilder  -> implementation


let wrap_method (type a') (type b') (m : (_, a', b')  method_t) (f0: a' -> b' Lwt.t) =
  (* Produce a method first-class module *)
  (module struct
    type request = a'
    type response = b'
    type request_g = request g
    type response_g = response g
    type response_t = response Lwt.t

    let request = m.request
    let response = m.response
    let call = f0
  end : Method with 
    type request = a' and
    type response = b' and
    type request_g = a' g and
    type response_g = b' g and
    type response_t = b' Lwt.t
  )


(* let declare : 
  type a b. ('i i c, a, b) method_t -> (a -> b Lwt.t) -> 'i ibuilder -> 'i ibuilder =
    fun m f b ->
      b.methods.(0) <- Some (Method (wrap_method m f));
      b 
      *)

(* An implementation is going to take a struct and return a struct.  Each of
 * these structs may contain capabilities. Each capability refers to an
 * implementation. The *setting* of the .
 *
 * An implmentation is a capability and must have the same type (t i c). It is
 * subject to reference counting.
 *
 *
 *
 * *)

let implement : type a. a i g -> (a ibuilder -> a ibuilder) -> implementation = 
  fun interface f ->
    match interface with 
    | Interface (_, n, _) -> Implementation ({interface; methods=Array.make n None} |> f)
    | _ -> failwith "Must be an interface"


type 'a peer = {
  (* We use an integer for the question_id. It seems *)
  fd : Lwt_unix.file_descr;
  mutable question_id : int32;
  mutable questions : Rpc.Return.t Lwt.u Int32Map.t;
  mutable imports : int Int32Map.t;
  mutable exports : impl Int32Map.t;
  bootstrap : 'a i g option
}


let peer () =
  let open Lwt_unix in
  let fd = socket PF_INET SOCK_STREAM 0 in
  let question_id = 0l in
  let questions = Int32Map.empty in
  let exports = Int32Map.empty in
  let imports = Int32Map.empty in
  let bootstrap = None in
  { fd; question_id; questions; exports; bootstrap; imports }

let peer_connect peer = 
  let open Lwt_unix in
  let%lwt address = 
    getaddrinfo "localhost" "60000" [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] in
  let address = address |> List.hd in
  let%lwt _ = connect peer.fd address.ai_addr in
  Lwt.return ()

let peer_loop peer = 
  let buf = Lwt_bytes.create 2048 in
  (* TODO: REMOVE HACKY HACK *)

  let rec f () = 
    let open Capnptk in
    let%lwt n = Lwt_bytes.recv peer.fd buf 0 (Lwt_bytes.length buf) [] in
    match n with
    | 0 ->  f ()
    | n -> 
        (
        let message = (Utils.decode Rpc.Message.t (Uint8Array1.of_bytes (Bigarray.Array1.sub buf 0 n))) in

        let open Rpc in
        match message => Message.union with
        | Return r -> 
            let qid = Declarative.(r => Return.answerId) in
            Lwt.wakeup_later (peer.questions |> Int32Map.find qid) r;
            f ()
        | Abort e -> failwith (e => Rpc.Exception.reason)
        | _ -> f ())
    in
    f ()







let interface_id = function | Ptr (Interface(_, _, id)) -> id | _ -> failwith "Not interface"

let server (xs : implementation array) = 
  xs |> (Int64Map.empty |> Array.fold_left (fun xs x -> 
    let Implementation b = x in
    let id = match b.interface with 
    | Interface (_, _, id) -> id
    | _ -> failwith "Not an interface." in
    Int64Map.add id x xs))


type ('a, 'p) chainable = 'a Lwt.t * int32 * 'p peer * Rpc.PromisedAnswer.Op.union list

let chainable_map : ('a -> 'b) -> ('a, 'p) chainable ->  ('b, 'p) chainable Lwt.t = 
  fun f (x, n, p, ops) -> 
  let open Lwt.Infix in
  Lwt.return (x >>= (fun x -> Lwt.return (f x)), n, p, ops)


let inc_ref xs k = 
  let v = try
    1 + (Int32Map.find k xs)
  with
    | Not_found -> 1
  in
  Int32Map.add k v xs

  
 
let set_caps caps c =
  {c with caps=caps |> Array.map (cast_struct (Rpc.CapDescriptor.t)  (Ptr Void))}

let get_caps c =
  c.caps |> Array.map (cast_struct (Ptr Void) (Rpc.CapDescriptor.t))

let process_return : type t. t c g -> _ peer -> Rpc.Return.t -> t c Lwt.t =
  fun typ peer answer ->
    peer |> ignore;
    match answer => Rpc.Return.union with
    | Results payload -> 
        (* There may be capabilties to insert into the table. *)

        let caps = payload => Rpc.Payload.capTable in

        (payload => Rpc.Payload.capTable |> Array.iter (
          fun x -> 
            match x => Rpc.CapDescriptor.union with
          | None -> ()
          | SenderHosted id ->
              peer.imports <- inc_ref peer.imports id
          | _ ->
              failwith "foo"
        ));

        Lwt.return (payload => (cast_field Rpc.Payload.content typ) |> set_caps caps)
    | Exception e ->
        failwith (e => Rpc.Exception.reason)
    | _ -> failwith "not recog"
  (* Process a return *)

let question : type t. (int32 -> Rpc.Message.t) -> t c g -> 'p peer -> (t c, 'p) chainable Lwt.t = 
  fun msg typ peer ->
    (* Fire a question, register a resolver in the question table. *)
    let question_id = peer.question_id in
    peer.question_id <- Int32.add 1l peer.question_id;

    let promise, resolver = Lwt.task () in

    (* add the resolver into the question map *)
    peer.questions <- peer.questions |> Int32Map.add question_id resolver;

    (* Fire the question *)
    let msg = question_id |> msg |> Capnptk.Utils.struct_to_bytes |> Uint8Array1.to_bytes in

    (* Prepare a future for the result *)

    let len = (Lwt_bytes.length msg) in
    let%lwt _ = Lwt_bytes.send peer.fd msg 0 len [] in
    (Lwt.bind promise (process_return typ peer), question_id, peer, []) |> Lwt.return



let bootstrap : type a. a i c g -> 'p peer -> (a i c, 'p) chainable Lwt.t =
  fun t peer -> 
    let msg question_id = build Rpc.Message.t (
      set Rpc.Message.union (
        Bootstrap (build Rpc.Bootstrap.t (
          set Rpc.Bootstrap.questionId question_id)))) in
    question msg t peer



let dispatch : _ peer -> Rpc.Call.t -> unit c Lwt.t =
  fun server call -> 
    match call => Rpc.Call.target => Rpc.MessageTarget.union with
    | ImportedCap c ->
        let e = server.exports |> Int32Map.find c in
        let module I = (val e) in
        failwith ""
        (* let iface = I.methods |> Int64Map.find (call => Rpc.Call.interfaceId) in
        begin match iface.(call => Rpc.Call.methodId) with 
        | Some Method m -> 
          let module M = (val m) in

          let v = call => Rpc.Call.params => (cast_field Rpc.Payload.content M.request) in 
          Lwt.bind (M.call v) (fun x -> x |> cast_struct M.response (Ptr Void) |> Lwt.return)
        | _ -> 
        failwith "bar"
        end
        *)
    | _ -> failwith "not supported yet"
    (* |> fun (Implementation iface) -> 
      let (IMethod (m, f)) = iface.methods |> IntMap.find (call => Rpc.Call.methodId) in
      let%lwt v = call => Rpc.Call.params => (cast_field Rpc.Payload.content m.request) |> f in
      v |> cast_struct m.response (Ptr Void) |> Lwt.return *)

(* Functions can be composed *)

let make_call : type a b. ('i, a, b) method_t -> a -> Rpc.MessageTarget.union -> int32 -> Rpc.Message.t =
  fun m x target qid ->
    build Rpc.Message.t (set Rpc.Message.union (
    Call (build Rpc.Call.t (fun b -> b |>
    set Rpc.Call.methodId m.method_id |>
    set Rpc.Call.questionId qid |>
    set Rpc.Call.interfaceId (m.iface |> interface_id) |>
    set Rpc.Call.target Rpc.(build MessageTarget.t (fun b -> b |> set MessageTarget.union target)) |>
    set Rpc.Call.params (build Rpc.Payload.t 
      (set (cast_field Rpc.Payload.content m.request) x)
    )))))


let call : type a b. ('i, a s c, b s c) method_t -> a s c -> ('i, 'p) chainable -> (b s c, 'p) chainable Lwt.t =
  fun m x (promise,qid,peer,ops) ->
    ops |> ignore;
    (* Basically prepare a call, send it, *)
    match Lwt.state promise with
    | Sleep -> question (make_call m x (PromisedAnswer Rpc.PromisedAnswer.(build t (fun b -> 
        b |> set questionId qid |>
        (ops |> List.map (fun z -> build Op.t (set Op.union z)) |> List.rev |> Array.of_list |> set transform )
        )))) m.response peer
    | Return v -> 
        let n = v |> get_interface_capability |> Int32.to_int in
        (match (v |> get_caps).(n) => Rpc.CapDescriptor.union with
        | SenderHosted n -> 
            question (make_call m x (ImportedCap n)) m.response peer
        | _ -> failwith "does it?")
    | _ -> failwith "not expected"

let push_op x (a, b, c, d) = (a, b, c, x :: d)

let csync (a, b, c, d) =
  let open Lwt.Infix in
  a >>= (fun _ -> Lwt.return (a, b, c, d))

let ptrField : ('s, 'a) field -> ('s c, 'p) chainable -> ('a, 'p) chainable Lwt.t =
  fun field chain ->
    match field with 
    | PtrField (n, _, _) -> chain |> push_op (Rpc.PromisedAnswer.Op.GetPointerField n) |> chainable_map (get field)
    | _ -> failwith "not a pointer field"
