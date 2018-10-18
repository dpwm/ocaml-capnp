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
  before: int;
  after: int;
  path: path;
  tree: tree;
}

let tree_length = function
  | Buffer data -> (Data.length [@inlined]) data
  | Join (a, b, _, _) -> a + b [@@inline]

let rec iter f = function
  | Buffer data -> f data
  | Join (_, _, x, y) ->
    iter f x;
    iter f y

let buffer x = Buffer x

(* let flatten x = x |> to_data |> of_data *)


let cat a b = Join (tree_length a, tree_length b, a, b)

let change tree x = {x with tree}

let left {tree; path; before; after} = match path with
  | Root -> failwith "Cannot go left from root"
  | Left _ -> failwith "Cannot go left of left branch"
  | Right {left; parent} ->
    {tree=left; path=Left {right=tree; parent};
     before=(before - tree_length left);
     after=(after + (tree_length tree) );
    }

let right {tree; path; before; after} = match path with
  | Root -> failwith "Cannot go right from root"
  | Right _ -> failwith "Cannot go right from right branch"
  | Left {right; parent} ->
    {tree=right; path=Right {left=tree; parent};
     before=(before + tree_length tree);
     after=(after - tree_length right)
    }

let up {tree; path; before; after} = match path with
  | Root -> failwith "Cannot go up from root"
  | Left {parent; right} ->
    let ll = tree_length tree in
    let lr = tree_length right in
    {tree=Join(ll, lr, tree, right); path=parent;
     before; after=after - lr
    }
  | Right {parent; left} ->
    let ll = tree_length left in
    let lr = tree_length tree in
    {tree=Join(ll, lr, left, tree); path=parent;
     after; before=before - ll}

let d1 {tree; path; before; after; _} = match tree with
  | Buffer _ -> failwith "Cannot go down from leaf node"
  | Join (a, b, left, right) ->
    {tree=left; path=Left {right; parent=path};
     before; after=after + b}

let d2 {tree; path; before; after; _} = match tree with
  | Buffer _ -> failwith "Cannot go down from leaf node"
  | Join (a, b, left, right) ->
    {tree=right; path=Right {left; parent=path};
     after; before=before + a}

let of_tree tree = {tree; path=Root; before=0; after=0}


let full_length {before; after; tree; _} =
  before + after + tree_length tree

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

let find n x =
  let rec f n x =
    let v = n - x.before in
    let v' = v - (tree_length [@inlined]) x.tree in
    match x.tree with
    | Buffer _ when v >= 0 && v' < 0 ->
      x
    | Join (a, _, _, _) when v >= 0 && v < a ->
      f n (d1 x)
    | Join (a, b, _, _) when v >= 0 && (v - a) < b ->
      f n (d2 x)
    | _ when n >= full_length x ->
      raise @@ Invalid_argument "Out of range"
    | _ -> x |> up |> f n
  in
  let check_in_range n x =
    if n < x.before then f n x else x
   in
   (f [@unrolled 2] [@specialized]) n x

let map f x =
  match x.tree with
  | Buffer x -> f x
  | _ -> failwith "Cannot map over join" [@@inline]

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
