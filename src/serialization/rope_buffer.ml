(* This is a hybrid functional data structure based on rope zippers *)

let alloc size = Data.make Data.bigstring size |> Rope.buffer 

type t = {
  pos : int;
  len : int;
  rope : Rope.t;
  spare : Rope.t;
  alloc_size : int;
  }

let rope_append r x =
  let open Rope in
  change (cat x.tree r) x

let expand x =
  let open Rope in
  let buf = alloc x.alloc_size in
  let alloc_size = x.alloc_size * 2 in
  {x with alloc_size; rope=x.rope |> find (x.len - 1) |> rope_append buf |> find x.pos}


let make ?(alloc_size=1024) () =
  {
  pos = 0;
  len = 0;
  alloc_size;
  rope = Rope.empty;
  spare = Rope.empty;
  } |> expand

let map_rope f x =
  let open Rope in
  match x.tree with
  | Buffer x -> f x
  | _ -> failwith "Cannot map over join"

let rope_of_data x =
  let open Rope in
  x |> buffer |> of_tree

let trim x =
  let buf, _ = map_rope (fun v -> Data.split v x.len) x.rope in
  let rope = rope_of_data buf in
  {x with rope}

let check_space_for n = function
  | x when x.len - x.pos < n ->
    x
  | x ->
    trim x |> expand

let skip n x =
  let x = x |> check_space_for n in
  {x with pos = x.pos + n}

let to_cursor x =
  {x with rope = Rope.find x.pos x.rope}

let write_string s x =
  (* Write the string at the cursor. *)
  let open Rope in
  let x = to_cursor x in
  let len = String.length s in
  let l, r = map_rope (fun v -> Data.split v (x.pos - x.rope.before)) x.rope in
  let r = (cat (buffer (Data.from Data.string s)) (buffer r)) in

  let rope = change (cat (buffer l) r) x.rope in
  {x with rope; pos = x.pos + len; len = x.len + len}
