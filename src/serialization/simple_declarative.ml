type 'a structure = Structure of { dwords : int; pwords : int }

module Counter = struct
  type t = {
    mutable n : int;
    limit : int;
  }

  let increment c n = c.n <- c.n + n; c.n > c.limit

  let make limit = {limit; n=0}
end

type 's c = {
  stream : Fstream.ByteStream.t;
  depth_count : Counter.t;
}

module ListProxy = struct
  type ('s, 'a) t = {
    cursor: 's c;
    get : int -> 's c -> 'a
  }

  let make cursor get = {cursor; get}
end

type 's seeker = 's c -> int -> unit

type ('s, 'a) getter = int -> 'a option -> 's c -> 'a

type ('s, 'a) setter = int -> 'a option -> 's c -> 'a -> unit

type ('s, 'a) field = {
  seeker : 's seeker ;
  getter : ('s, 'a) getter ;
  setter : ('s, 'a) setter ;
  default : 'a option ;
  offset : int ;
}

let null_setter _ = failwith ""

let data_seeker _n _c =
  failwith "seek"

let bit_seeker _ _ = failwith "seek"

let ptr_seeker _ _ = failwith "seek"

let int64 (_s : 's structure) ?default offset : ('s, int64) field = 
  let getter _ _ _ = 0L in 
  let setter _ _ _ _ = () in
  { setter; seeker=data_seeker; getter; default; offset }

let list (_s : 's structure) (_t : 't) ?default offset : ('s, ('s, 't) ListProxy.t) field =
  let getter _ _ cursor = ListProxy.make cursor (fun _ _ -> failwith "") in
  let setter _ _ _ _ = () in
  {getter; setter; seeker=ptr_seeker; offset; default}

let structure (_s : 's structure) (_t : 't structure) ?default offset : ('s, 't c) field =
  let getter _ _ _ = failwith "undefined" in
  let setter _ _ _ _ = () in
  {getter; setter; seeker=ptr_seeker; offset; default}

let get : 's c -> ('s, 'a) field -> 'a =
  fun c f ->
  f.seeker c f.offset;
  f.getter f.offset f.default c
