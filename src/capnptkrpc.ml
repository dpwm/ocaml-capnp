
open Capnptk.Declarative

type client

module Uint8Array1 = struct
  open Bigarray

  type t = (int, int8_unsigned_elt, c_layout) Array1.t

  external of_bytes : Lwt_bytes.t -> t = "%identity"
  external to_bytes : t -> Lwt_bytes.t = "%identity"
end

(* A server is a collection of implementations *)
module Int = struct
  type t = int
  let compare a b = b - a
end


module IntMap = Map.Make(Int)
module Int32Map = Map.Make(Int32)
module Int64Map = Map.Make(Int64)

type imethod = IMethod : ('i, 'a, 'b) method_t * ('a -> 'b Lwt.t) -> imethod

type 'a ibuilder = {
  interface: 'a i g;
  methods : imethod IntMap.t
}

type implementation = Implementation : 'a ibuilder  -> implementation

let declare : 
  type a b. ('i i c, a, b) method_t -> (a -> b Lwt.t) -> 'i ibuilder -> 'i ibuilder =
    fun m f b ->

      let m' = IMethod(m, f) in
      let methods = b.methods |> IntMap.add m.method_id m' in
      {b with methods}

let implement : type a. a i g -> (a ibuilder -> a ibuilder) -> implementation = 
  fun interface f ->
    match interface with 
    | Interface _ -> Implementation ({interface; methods=IntMap.empty} |> f)
    | _ -> failwith "Must be an interface"


type 'a peer = {
  (* We use an integer for the question_id. It seems *)
  fd : Lwt_unix.file_descr;
  mutable question_id : int32;
  mutable questions : Rpc.Return.t Lwt.u Int32Map.t;
  mutable imports : int Int32Map.t;
  implementations : implementation Int64Map.t;
  bootstrap : 'a i g option
}


let peer () =
  let open Lwt_unix in
  let fd = socket PF_INET SOCK_STREAM 0 in
  let question_id = 0l in
  let questions = Int32Map.empty in
  let implementations = Int64Map.empty in
  let imports = Int32Map.empty in
  let bootstrap = None in
  { fd; question_id; questions; implementations; bootstrap; imports }

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

let chainable_map : ('a -> 'b) -> ('a, 'p) chainable ->  ('b, 'p) chainable = 
  fun f (x, n, p, ops) -> 
  let open Lwt.Infix in
  (x >>= (fun x -> Lwt.return (f x)), n, p, ops)


let inc_ref xs k = 
  let v = try
    1 + (Int32Map.find k xs)
  with
    | Not_found -> 1
  in
  Int32Map.add k v xs

  
 

let process_return : type t. t c g -> _ peer -> Rpc.Return.t -> t c Lwt.t =
  fun typ peer answer ->
    peer |> ignore;
    match answer => Rpc.Return.union with
    | Results payload -> 
        (* There may be capabilties to insert into the table. *)

        (payload => Rpc.Payload.capTable |> Array.iter (
          fun x -> 
            match x => Rpc.CapDescriptor.union with
          | SenderHosted id ->
              peer.imports <- inc_ref peer.imports id
          | None -> ()
          | _ ->
              failwith "foo"
        ));

        Lwt.return (payload => (cast_field Rpc.Payload.content typ))
    | Exception e ->
        failwith (e => Rpc.Exception.reason)
    | _ -> failwith "not recog"
  (* Process a return *)

let question : type t. (int32 -> Rpc.Message.t) -> t c g -> 'p peer -> (t c, 'p) chainable = 
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
    Lwt_bytes.send peer.fd msg 0 len [] |> ignore;
    (Lwt.bind promise (process_return typ peer), question_id, peer, [])



let bootstrap : type a. a i c g -> 'p peer -> (a i c, 'p) chainable =
  fun t peer -> 
    let msg question_id = build Rpc.Message.t (
      set Rpc.Message.union (
        Bootstrap (build Rpc.Bootstrap.t (
          set Rpc.Bootstrap.questionId question_id)))) in
    question msg t peer




let dispatch : _ peer -> Rpc.Call.t -> unit c Lwt.t =
  fun server call -> 
    server.implementations |> 
    Int64Map.find (call => Rpc.Call.interfaceId) |> fun (Implementation iface) -> 
      let (IMethod (m, f)) = iface.methods |> IntMap.find (call => Rpc.Call.methodId) in
      let%lwt v = call => Rpc.Call.params => (cast_field Rpc.Payload.content m.request) |> f in
      v |> cast_struct m.response (Ptr Void) |> Lwt.return

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


let call : type a b. ('i, a s c, b s c) method_t -> a s c -> ('i, 'p) chainable -> (b s c, 'p) chainable =
  fun m x (promise,qid,peer,ops) ->
    ops |> ignore;
    (* Basically prepare a call, send it, *)
    match Lwt.state promise with
    | Sleep -> question (make_call m x (PromisedAnswer Rpc.PromisedAnswer.(build t (fun b -> 
        b |> set questionId qid |>
        (ops |> List.map (fun z -> build Op.t (set Op.union z)) |> List.rev |> Array.of_list |> set transform )
        )))) m.response peer
    | Return v -> 
        
        question (make_call m x (ImportedCap (v |> get_interface_capability))) m.response peer
    | _ -> failwith "not expected"

let push_op x (a, b, c, d) = (a, b, c, x :: d)

let csync (a, b, c, d) = (a, b, c, d)

let ptrField : ('s, 'a) field -> ('s c, 'p) chainable -> ('a, 'p) chainable =
  fun field chain ->
    match field with 
    | PtrField (n, _, _) -> chain |> push_op (Rpc.PromisedAnswer.Op.GetPointerField n) |> chainable_map (get field)
    | _ -> failwith "not a pointer field"
