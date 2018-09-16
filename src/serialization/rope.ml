(* Nearly all our complexity comes from our half-baked multi-copy approach to
 * allocation.*)

(* We can make our interface approximately zero-copy by merging the properties
 * of a tree with the properties of a byte array. It will be posisble to use
 * this in a zero-copy way, using iter, and producing an iovector. For windows
 * support, we will have to resort to collapsing. *)

(* #load "data.cmo" *)

type tree =
  | Buffer of Data.t
  | Join of int * int * tree * tree

type path =
  | Root
  | Left of {parent: path; right: tree}
  | Right of {parent: path; left: tree}

type t = {
  path: path;
  tree: tree;
  before: int;
  length: int;
  after: int;
}

let length = function
  | Buffer data -> Data.length data
  | Join (a, b, _, _) -> a + b

let rec iter f = function
  | Buffer data -> f data
  | Join (_, _, x, y) ->
      iter f x;
      iter f y

let buffer x = Buffer x

(* let flatten x = x |> to_data |> of_data *)


let cat a b = Join (length a, length b, a, b)

let change tree x = {x with tree; length=length tree}

let left {tree; path; before; after; length=len} = match path with
  | Root -> failwith "Cannot go left from root"
  | Left _ -> failwith "Cannot go left of left branch"
  | Right {left; parent} ->
    let length = length left in
    {tree=left; path=Left {right=tree; parent};
      before=(before - length);
     after=(after + len);
     length
    }

let right {tree; path; before; after; length=len} = match path with
  | Root -> failwith "Cannot go right from root"
  | Right _ -> failwith "Cannot go right from right branch"
  | Left {right; parent} ->
    let length = length right in
    {tree=right; path=Right {left=tree; parent};
      before=(before + len);
     after=(after - length);
     length
    }

let up {tree; path; before; after; length=len} = match path with
  | Root -> failwith "Cannot go up from root"
  | Left {parent; right} ->
    let ll = len in
    let lr = length right in
    {tree=Join(ll, lr, tree, right); path=parent;
     before; after=after - lr;
     length = ll + lr
    }
  | Right {parent; left} ->
    let lr = len in
    let ll = length left in
    {tree=Join(ll, lr, left, tree); path=parent;
     after; before=before - ll;
     length=ll + lr}

let d1 {tree; path; before; after; _} = match tree with
  | Buffer _ -> failwith "Cannot go down from leaf node"
  | Join (a, b, left, right) ->
    {tree=left; path=Left {right; parent=path};
      before; after=after + b; length=a}

let d2 {tree; path; before; after; _} = match tree with
  | Buffer _ -> failwith "Cannot go down from leaf node"
  | Join (a, b, left, right) ->
    {tree=right; path=Right {left; parent=path};
      after; before=before + a; length=b}

let of_tree tree =
  {tree; path=Root; before=0; after=0; length=length tree}


let full_length {before; after; length; _} = before + after + length

let rec sink_left x =
  match x.tree with
  | Buffer _ -> x
  | Join _ -> x |> d1 |> sink_left

let rec sink_right x =
  match x.tree with
  | Buffer _ -> x
  | Join _ -> x |> d2 |> sink_right

let rec float_left x =
  match x.path with
  | Root -> x
  | Left _ -> x |> up |> float_left
  | Right _ -> x |> left

let rec float_right x =
  match x.path with
  | Root -> x
  | Left _ -> x |> right
  | Right _ -> x |> up |> float_right

let rec float_top x =
  match x.path with
  | Root -> x
  | _ -> x |> up |> float_top

let to_tree x =
  (x |> float_top).tree

let rec find n x =
  let v = n - x.before in
  let v' = v - x.length in
  match x.tree with
  | Buffer _ when v >= 0 && v' < 0 ->
    x
  | _ -> begin match x.tree with
    | Join (a, _, _, _) when v >= 0 && v < a ->
      x |> d1 |> find n
    | Join (a, b, _, _) when v >= 0 && (v - a) < b ->
      x |> d2 |> find n
    | _ when n >= full_length x ->
      raise @@ Invalid_argument "Out of range"
    | _ -> x |> up |> find n
  end

let map f x =
  match x.tree with
  | Buffer x -> f x
  | _ -> failwith "Cannot map over join"

let empty = of_tree (buffer Data.(from string ""))

let to_buffer buf x =
  let rec f x =
    map (Data.to_buffer buf) x;
    let x' = x |> float_right in
    match x'.path with
    | Root -> ()
    | _ -> x' |> sink_left |> f
  in
  x |> float_top |> sink_left |> f

let to_string x =
  let len = full_length x in
  if len = 0 then "" else
  let buf = Buffer.create (full_length x) in
  x |> find 0 |> to_buffer buf;
  Buffer.contents buf

let get n x =
  let x = find n x in
  let m = n - x.before in
  map (fun d -> Data.get d m) x

let rec pp_tree ppf =
  let fmt f = Format.fprintf ppf f in
  function
  | Join (a, b, l, r) ->
    fmt "@[<v 2>Join (%d, %d,@ " a b;
    pp_tree ppf l;
    fmt ",@ ";
    pp_tree ppf r;
    fmt ")@]"
  | Buffer x ->
    fmt "Buffer (Data.(from string)) %S" (Data.to_string x)

let rec pp_path ppf =
  let fmt f = Format.fprintf ppf f in
  function
  | Root ->
    fmt "Root"
  | Left x ->
    fmt "Left {parent=";
    pp_path ppf x.parent;
    fmt "; right=";
    pp_tree ppf x.right;
    fmt "}";
  | Right x -> 
    fmt "Right {parent=";
    pp_path ppf x.parent;
    fmt "; left=";
    pp_tree ppf x.left;
    fmt "}"

let append r x =
  change (cat x.tree r) x

let pp ppf x =
  let fmt f = Format.fprintf ppf f in
  fmt "@[<v>{@[<v 2>@ path=";
  pp_path ppf x.path;
  fmt ";@ tree=";
  pp_tree ppf x.tree;
  fmt ";@ before=%d;@ after=%d@]@ }@]" x.before x.after

let slice_to n x =
  match n with
  | n when full_length x = n -> x
  | n when n < 0  || n > full_length x -> raise (Invalid_argument "Out of range")
  | n ->
    let x = find n x in
    let m = n - x.before in
    let a, _ = map (fun x -> Data.split x m) x in
    let x = change (a |> buffer) x in

    let rec f x =
      (* climb up the subtree. If we find a left branch taken, jump it. *)
      match x.after with
        | _ ->
          match x.path with
          | Left _ -> x |> up |> change x.tree |> f
          | Right _ -> x |> up |> f
          | Root -> x
    in

    let o = x |> f in

    if o.before != 0 then failwith "Before != 0";
    if o.after != 0 then failwith (Printf.sprintf "After = %d != 0" x.after);
    if full_length o != n then failwith "Length does not match";
    o
