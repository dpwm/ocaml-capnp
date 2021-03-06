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

(* We now just store the ibuilder type *)
type lwt


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




type 'a ibuilder = {
  interface: 'a i g;
  methods : stored_method option array Int64Map.t;
  id : int;
} and 
('i, 'a, 'b) implemented_method = {
  m : ('i, 'a, 'b) method_t;
  f : ('a -> 'b Lwt.t)
} and
stored_method = StoredMethod : ('i i c, 'a, 'b) implemented_method  -> stored_method

type implementation = Implementation : 'a ibuilder  -> implementation


(* This is simpler and better than the alternative *)
external to_ximpl : 'i ibuilder -> (lwt, 'i) ximpl_t = "%identity"
external of_ximpl : (_, 'i) ximpl_t -> 'i ibuilder = "%identity"

let get_interface : type a. a i c g -> _ =
  function 
    | Ptr Interface x -> x
    | _ -> failwith "not an interface"

let declare : 
  type a b. ('i i c, a, b) method_t -> (a -> b Lwt.t) -> 'i ibuilder -> 'i ibuilder =
    fun m f b ->
      (* What if we already have it? *)
      let _, nmeth, iid = get_interface m.iface in
      let mtab, b = try Int64Map.find iid b.methods, b with | Not_found ->
        let xs = (Array.make nmeth None) in
        xs, {b with methods=Int64Map.add iid xs b.methods};
      in
      mtab.(m.method_id) <- Some (StoredMethod {m; f});
      b

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



let unptr = function | Ptr x -> x | _ -> failwith "foo"

type 'a refcounted = 'a * int
let addref n (x, c) = (x, c + n)
let incref (x, c) = addref (1) (x, c)
let refcounted ?(n=1) x = (x, n)
let refcounted_get (x, _) = x


type 'a peer = {
  (* We use an integer for the question_id. It seems *)
  fd : Lwt_unix.file_descr;
  mutable question_id : int32;
  mutable questions : Rpc.Return.t Lwt.u Int32Map.t;

  mutable answers : Rpc.Return.t Lwt.t Int32Map.t;

  mutable imports : int Int32Map.t;
  (* Exports is interesting. We want both directions to be mappable. *)
  mutable export_id : int;
  mutable exports : ximpl refcounted IntMap.t;
  mutable implementations : int IntMap.t;
  bootstrap : 'a i c option
}

(* If we could . The problem is that the types are going to be unknown. *)
(* A function on the plain side could assist with registering the table and
 * avoid having to deal with horrible types. *)


let implementation_id =
  let x = ref 0 in
  let inc () = 
    let x' = !x in
    x := x' + 1;
    x'
  in
  inc

let implement : type a. a i c g -> (a ibuilder -> a ibuilder) -> a i c = 
  fun interface f ->
    (* We want to create an interface with just a cap ptr *)
    let ximpl = 
      {interface=unptr interface; methods=Int64Map.empty; id=implementation_id ()} |> f |> to_ximpl in

    cap_ptr (XImpl ximpl)


let peer ?fd ?bootstrap () =
  let fd = match fd with | Some fd -> fd | None -> 
    Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let question_id = 0l in
  let questions = Int32Map.empty in
  let exports = IntMap.empty in
  let imports = Int32Map.empty in
  let implementations = IntMap.empty in
  let answers = Int32Map.empty in
  let export_id = 0 in
  let bootstrap = bootstrap in
  { fd; question_id; questions; exports; bootstrap; imports; export_id; answers; implementations }

let peer_connect peer = 
  let open Lwt_unix in
  let%lwt address = 
    getaddrinfo "localhost" "60000" [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] in
  let address = address |> List.hd in
  let%lwt _ = connect peer.fd address.ai_addr in
  Lwt.return ()



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
    Lwt_log.debug "Sending question";%lwt
    let%lwt n = Lwt_bytes.write peer.fd msg 0 len in
    Lwt_log.debug_f "Done %d" n;%lwt
    (Lwt.bind promise (process_return typ peer), question_id, peer, []) |> Lwt.return


let cap_get_id x =
  let XImpl v = x in
  let v = v |> of_ximpl in
  v.id

let bootstrap : type a. a i c g -> 'p peer -> (a i c, 'p) chainable Lwt.t =
  fun t peer -> 
    let msg question_id = build Rpc.Message.t (
      set Rpc.Message.union (
        Bootstrap (build Rpc.Bootstrap.t (
          set Rpc.Bootstrap.questionId question_id)))) in
    question msg t peer

let inc_capability_ref peer c =
  (* We will take a capability.*) 
  let id = cap_get_id c in

  let capid = try
    peer.implementations |> IntMap.find id 
  with  Not_found ->
    let v = peer.export_id in
    peer.export_id <- 1 + peer.export_id;
    peer.implementations <- IntMap.add id v peer.implementations;
    v
  in

  (* We must increment the reference count *)
  begin try 
    let r = IntMap.find capid peer.exports in
    peer.exports <- IntMap.add capid (incref r) peer.exports
  with Not_found ->
    peer.exports <- IntMap.add capid (refcounted c) peer.exports
  end;
  

  Rpc.CapDescriptor.SenderHosted (Int32.of_int capid)


let payload peer biface = 
  Rpc.Payload.(build t (fun x -> 
    x |> 
    set content (to_void biface) |>
    set capTable ( 
      biface.impls |> snd |> List.rev |> List.map (
        fun ximpl -> 
          let XImpl v = ximpl in
          let v = v |> of_ximpl in
          Lwt.async (fun () -> Lwt_log.debug_f "With id: %d" v.id);
          (* Store the capability. *)
          let u = inc_capability_ref peer ximpl in
          Rpc.CapDescriptor.(build t (fun v -> v |> set union u))
    ) |> Array.of_list
    )
    )
  )

let send_msg peer x =
  let msg = x |> Capnptk.Utils.struct_to_bytes |> Uint8Array1.to_bytes in
  let len = (Lwt_bytes.length msg) in
  let%lwt n = Lwt_bytes.write peer.fd msg 0 len in
  Lwt_log.debug_f "Sent %d bytes" n;%lwt
  Lwt.return ()


let send_return peer v =
  let msg = Rpc.Message.(build t
  (fun x -> 
    x |> 
    set Rpc.Message.union (Return v))) in
  send_msg peer msg;%lwt
  Lwt.return v

let return_ok ?(release=true) qid payload = 
  Rpc.Return.(build t (fun x ->
  x |> 
  set union (Results (
    payload
    )) |>
  set answerId qid |> 
  set releaseParamCaps release
))

let impl_call_method peer ximpl iid mid params =
  let XImpl v = ximpl in
  let v = v |> of_ximpl in

  match (v.methods |> Int64Map.find iid).(mid) with
  | Some (StoredMethod {m; f}) -> 
      let x = params |> cast_struct (Ptr Void) m.request in
      let%lwt x = f x in
      let x = cast_struct m.response (Ptr Void) x in
      payload peer x |> Lwt.return
  | None -> 
      failwith "Method not implemented"

let store_answer peer answer_id =
  let promise, resolver = Lwt.wait() in
  peer.answers <- peer.answers |> Int32Map.add answer_id promise; 
  promise, resolver

let get_return_payload r =
  match r => Rpc.Return.union with 
  | Results payload -> payload
  | _ -> raise Not_found

let dispatch : _ peer -> Rpc.Call.t -> unit Lwt.t =
  fun server call -> 
    server |> ignore;

    Lwt_log.debug_f "Call %ld" (call => Rpc.Call.questionId);%lwt
    match call => Rpc.Call.target => Rpc.MessageTarget.union with
    | ImportedCap c ->
        try
          let ximpl = server.exports |> IntMap.find (Int32.to_int c) |> refcounted_get in

          (* We must take this slightly convoluted path to make sure to avoid
           * race conditions with chained method calls. *)
          let _, resolver = store_answer server Rpc.Call.(call => questionId) in


          let f () =
            let%lwt v = 
              Rpc.Call.(impl_call_method server ximpl (call => interfaceId) (call => methodId) Rpc.Payload.(call => params => content)) in

            let%lwt return = v |> return_ok Rpc.Call.(call=>questionId) |> send_return server in
            Lwt.wakeup_later resolver return;
            Lwt.return ()

          in

          Lwt.async f;
          Lwt.return ()

          
        with Not_found -> 
          Lwt_log.debug "Capability not found"; ;

    | PromisedAnswer pA -> 
        let open Rpc.PromisedAnswer in

        let _, resolver = store_answer server Rpc.Call.(call => questionId) in

        let f () = 
          let%lwt answer = server.answers |> Int32Map.find (pA => questionId) in

          let payload = answer |> get_return_payload in

          let content = pA => transform |> Array.fold_left (fun v x -> match x => Op.union with
            | Noop -> v
            | GetPointerField n -> void_get_ptr n v
          ) (payload => Rpc.Payload.content |> c_read_ptr) in


          (* Use this to get a capability *)

          let i = ensure_cap_ptr content in

          let ximpl = match (payload => Rpc.Payload.capTable).(i) => Rpc.CapDescriptor.union with
          | SenderHosted c -> 
              server.exports |> IntMap.find (Int32.to_int c) |> refcounted_get
          | _ -> raise Not_found
          in

          let%lwt v = 
            Rpc.Call.(impl_call_method server ximpl (call => interfaceId) (call => methodId) Rpc.Payload.(call => params => content)) in

          let%lwt return = v |> return_ok Rpc.Call.(call=>questionId) |> send_return server in
          Lwt.wakeup_later resolver return;
          Lwt.return ()
        in
        Lwt.async f;
        Lwt.return ()
            

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
    Lwt_log.debug "Preparing call";%lwt
    match Lwt.state promise with
    | Sleep ->
        Lwt_log.debug "Sending deferred call";%lwt
        question (make_call m x (PromisedAnswer Rpc.PromisedAnswer.(build t (fun b -> 
        b |> set questionId qid |>
        (ops |> List.map (fun z -> build Op.t (set Op.union z)) |> List.rev |> Array.of_list |> set transform )
        )))) m.response peer
    | Return v -> 
        let n = v |> get_interface_capability |> Int32.to_int in
        Lwt_log.debug "Sending immediate call";%lwt
        (match (v |> get_caps).(n) => Rpc.CapDescriptor.union with
        | SenderHosted n -> 
            question (make_call m x (ImportedCap n)) m.response peer
        | _ -> failwith "does it?")
    | _ -> failwith "not expected"

let push_op x (a, b, c, d) = (a, b, c, x :: d)

let csync (a, b, c, d) =
  let open Lwt.Infix in
  a >>= (fun _ -> Lwt.return (a, b, c, d))

let result (a, _, _, _) = a

let ptrField : ('s, 'a) field -> ('s c, 'p) chainable -> ('a, 'p) chainable Lwt.t =
  fun field chain ->
    match field with 
    | PtrField (n, _, _) -> chain |> push_op (Rpc.PromisedAnswer.Op.GetPointerField n) |> chainable_map (get field)
    | _ -> failwith "not a pointer field"



let easy_read fd n =
  let buf = Lwt_bytes.create n in
  let rec loop o m = 
    (* Actually read *)
    let%lwt n = Lwt_bytes.read fd buf o n in
    if m = n then
      Lwt.return buf
    else
      let%lwt v = loop (o + n) (m - n) in
      Lwt.return v
  in
  let open Lwt.Infix in
  loop 0 n >|= Uint8Array1.of_bytes


external get_32 : Uint8Array1.t -> int -> int32 = "%caml_bigstring_get32"

let read_capnprpc_msg fd = 
  let%lwt header = easy_read fd 8 in
  let nsegs = get_32 header 0 |> Int32.to_int |> ((+) 1) in
  let seglengths = Array.make nsegs 0 in
  seglengths.(0) <- get_32 header 4 |> Int32.to_int;
  let total = ref seglengths.(0) in

  let%lwt b = easy_read fd ((nsegs - 1) * 4) in

  (* Force alignment *)
  let%lwt _ = if nsegs land 1 = 0 then 
    easy_read fd 4 
  else
    easy_read fd 0
  in

  if Array.length seglengths > 1 then
    for i = 0 to nsegs - 2 do
      let x = get_32 b (i * 4) |> Int32.to_int in
      total := !total + x;
      seglengths.(i+1) <- x;
    done;

  let%lwt data = easy_read fd (8 * !total)in

  Lwt.return (Capnptk.Utils.from_bytes Rpc.Message.t seglengths data)



let peer_loop peer = 
  let rec f () = 
    let open Lwt.Infix in
    let%lwt message = read_capnprpc_msg peer.fd in
    let open Capnptk in

    let open Rpc in
    match message => Message.union with
    | Return r -> 
        let qid = Declarative.(r => Return.answerId) in
        Lwt.wakeup_later (peer.questions |> Int32Map.find qid) r;
        f ()
    | Call c -> 
        Lwt_log.debug "A call!";%lwt
        dispatch peer c;%lwt
        f ()
    | Bootstrap b ->
        begin match peer.bootstrap with
        | Some biface ->
            biface |> ignore;
            let qid = Rpc.Bootstrap.(b => questionId) in
            Lwt_log.debug_f "bootstrap called %ld" qid;%lwt

            let _, resolver = store_answer peer qid in

            let answer = payload peer biface |> return_ok qid in
            let af () = 
              let%lwt r = send_return peer answer in
              Lwt.wakeup_later resolver r;
              Lwt.return ()
            in
            Lwt.async af;
            let%lwt _ = f () in
            Lwt.return ()
        | None -> failwith "bootstrap not set" 
        end
    | Abort e -> failwith (e => Rpc.Exception.reason)
    | _ -> () |> Lwt.return >>= f
    in
    let%lwt _ = f () in
    Lwt.return ()



let peer_serve peer = 
  let open Lwt_unix in
  let open Lwt.Infix in
  on_signal Sys.sigint (fun _ -> failwith "Caught SIGINT") |> ignore;
  on_signal Sys.sigterm (fun _ -> failwith "Caught SIGTERM") |> ignore;
  setsockopt peer.fd SO_REUSEADDR true;
  (* This is necessary so that we can relaunch the server. *)

  let%lwt address = 
    getaddrinfo "0.0.0.0" "60000" [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] in
  let address = address |> List.hd in
  let%lwt _ = bind peer.fd address.ai_addr in
  Lwt_log.debug "Listening on address";%lwt

  listen peer.fd 5;

  let rec f () = 
    
    let%lwt fd, sockaddr = accept peer.fd in
    let soi = Unix.string_of_inet_addr in
    let%lwt _ = (match sockaddr with 
    | ADDR_UNIX x -> Lwt_log.debug_f "Accepted connection from %s" x
    | ADDR_INET (x, v) -> Lwt_log.debug_f "Accepted connection from %s:%d" (soi x) v
    ) in
    
    Lwt.async (fun () -> try%lwt
      peer_loop { peer with fd }
      with 
      | e ->  (
        close fd;%lwt
        Lwt_log.debug_f "An error occured: %s\nBacktrace:\n%s" (Printexc.to_string e) (Printexc.get_backtrace ());%lwt
        Lwt_log.debug "Peer loop closed."
      ));
    f ()
  in
  Lwt.return () >>= f
