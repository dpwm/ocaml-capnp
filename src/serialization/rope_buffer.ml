(* This is a hybrid functional data structure based on rope zippers *)

let alloc size = Data.make Data.bigstring size |> Rope.buffer

type 'a t = {
  pos : int;
  len : int;
  rope : Rope.t;
  spare : Rope.t;
  alloc_size : int;
  result : 'a
  }

let expand x =
  let open Rope in
  let buf = alloc x.alloc_size in
  let len = x.len + x.alloc_size in
  let alloc_size = x.alloc_size * 2 in
  match x.len with
  | 0 ->
    {x with alloc_size; rope = of_tree buf |> find x.pos; len}
  | _ ->
  {x with alloc_size; len; rope=x.rope |> find (max 0 (x.len - 1)) |> Rope.append buf |> find x.pos}

let push v x = {x with result = (v, x.result)}

let make ?(alloc_size=1024) () =
  {
  pos = 0;
  len = 0;
  alloc_size;
  rope = Rope.empty;
  spare = Rope.empty;
  result = ();
  } |> expand



let trim x =
  {x with rope = Rope.slice_to x.pos x.rope}

let check_space_for n = function
  | x when n <= x.len - x.pos  ->
    x
  | x ->
    trim x |> expand

let skip n x =
  let x = x |> check_space_for n in
  {x with pos = x.pos + n}

let to_cursor x =
  let x = x |> check_space_for 1 in
  {x with rope = Rope.find x.pos x.rope
  }

let pos pos x =
  {x with pos} |> to_cursor

let write_string s x =
  (* Write the string at the cursor. *)
  let open Rope in
  let x = to_cursor x in
  let len = String.length s in
  let l, r = map (fun v -> Data.split v (x.pos - x.rope.before)) x.rope in
  let r = (cat (buffer (Data.from Data.string s)) (buffer r)) in

  let rope = change (cat (buffer l) r) x.rope in
  {x with rope; pos = x.pos + len; len = x.len + len} |> to_cursor

let advance n x =
  {x with pos = n + x.pos} |> to_cursor

let write f l n x =
  let open Rope in
  let x = x |> to_cursor |> check_space_for l in
  let m = x.pos - x.rope.before in
  map (f m n) x.rope;
  x |> advance l

let read f l x =
  let open Rope in
  let m = x.pos - x.rope.before in
  x |> push (map (f m) x.rope) |> advance l

let write_int64 n x =
  write (fun m n d -> Data.set_int64 d m n) 8 n x

let write_int32 n x =
  write (fun m n d -> Data.set_int32 d m n) 4 n x

let write_int16 n x =
  write (fun m n d -> Data.set_int16 d m n) 2 n x

let write_int8 n x =
  write (fun m n d -> Data.set_int8 d m n) 1 n x

let read_int64 x =
  read (fun m d -> Data.get_int64 d m) 8 x

let read_int32 x =
  read (fun m d -> Data.get_int32 d m) 4 x

let read_int16 x =
  read (fun m d -> Data.get_int16 d m) 2 x

let read_int8 x =
  read (fun m d -> Data.get_int8 d m) 1 x

let to_rope x =
  x.rope |> Rope.slice_to x.pos

let pp f x =
  let fmt x = Format.fprintf f x in
  fmt "@[<v>{@[<v 2>@ pos = %d;@ len = %d;@ rope = " x.pos x.len;
  Rope.pp f x.rope;
  fmt ";@ spare = ";
  Rope.pp f x.spare;
  fmt ";@ alloc_size = %d@]@ }@]" x.alloc_size
