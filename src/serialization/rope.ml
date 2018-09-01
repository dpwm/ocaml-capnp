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

let change tree x = {x with tree}

let left {tree; path; before; after} = match path with
  | Root -> failwith "Cannot go left from root"
  | Left _ -> failwith "Cannot go left of left branch"
  | Right {left; parent} ->
    {tree=left; path=Left {right=tree; parent};
      before=(before - length left);
      after=(after + length tree)}

let right {tree; path; before; after} = match path with
  | Root -> failwith "Cannot go right from root"
  | Right _ -> failwith "Cannot go right from right branch"
  | Left {right; parent} ->
    {tree=right; path=Right {left=tree; parent};
      before=(before + length tree);
      after=(after - length right)
    }

let up {tree; path; before; after} = match path with
  | Root -> failwith "Cannot go up from root"
  | Left {parent; right} ->
    {tree=Join(length tree, length right, tree, right); path=parent;
      before; after=after - (length right)
    }
  | Right {parent; left} ->
    {tree=Join(length left, length tree, left, tree); path=parent;
      after; before=before - (length left)}

let d1 {tree; path; before; after} = match tree with
  | Buffer _ -> failwith "Cannot go down from leaf node"
  | Join (_, b, left, right) ->
    {tree=left; path=Left {right; parent=path};
      before; after=after + b}

let d2 {tree; path; before; after} = match tree with
  | Buffer _ -> failwith "Cannot go down from leaf node"
  | Join (a, _, left, right) ->
    {tree=right; path=Right {left; parent=path};
      after; before=before + a}

let of_tree tree =
  {tree; path=Root; before=0; after=0}


let full_length {tree; before; after; _} = before + after + length tree

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
  if n >= full_length x then
    failwith "Out of range"
  else
    let v = n - x.before in
    match x.tree with
    | Buffer d when v >= 0 && v < Data.length d ->
      x
    | Join (a, _, _, _) when v >= 0 && v < a ->
      x |> d1 |> find n
    | Join (a, b, _, _) when v >= 0 && (v - a) < b ->
      x |> d2 |> find n
    | _ -> x |> up |> find n


let empty = of_tree (buffer Data.(from string ""))
