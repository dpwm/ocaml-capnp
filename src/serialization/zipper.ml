module Btree = struct
  type 'a t =
    | Leaf of 'a
    | Branch of 'a t * 'a t

  type 'a p =
    | Root
    | Left of 'a p * 'a t
    | Right of 'a t * 'a p

  type 'a z = Loc of 'a t * 'a p

  let change (Loc(_, p)) t = Loc (t, p)

  let left (Loc(t, p)) = match p with
    | Root -> failwith "left of root"
    | Left (_father, _right) -> failwith "left of left"
    | Right (left, father) -> Loc(left, Left(father, t))

  let right (Loc(t, p)) = match p with
    | Root -> failwith "right of root"
    | Left (father, right) -> Loc(right, Right(t, father))
    | Right _ -> failwith "right of right"

  let up (Loc(t, p)) = match p with
    | Root -> failwith "up of root"
    | Left (father, right) -> Loc(Branch(t, right), father)
    | Right (left, father) -> Loc(Branch(left, t), father)

  let d1 (Loc(t, p)) = match t with
    | Leaf _ -> failwith "d1 of leaf"
    | Branch (l, r) -> Loc(l, Left(p, r))

  let d2 (Loc(t, p)) = match t with
    | Leaf _ -> failwith "d2 of leaf"
    | Branch (l, r) -> Loc(r, Right(l, p))

  let rec sink_left x =
    let Loc (t, _) = x in
    match t with
    | Leaf _ -> x
    | Branch _ -> x |> d1 |> sink_left

  let rec sink_right x =
    let Loc (t, _) = x in
    match t with
    | Leaf _ -> x
    | Branch _ -> x |> d2 |> sink_right

  let rec float_right x =
    let Loc(_, p) = x in
    match p with
    | Root -> x
    | Left _ -> x |> right
    | Right _ -> x |> up |> float_right

  let first x = x |> sink_left
  let next x = x |> float_right |> sink_left

  let z_of_tree t =
    Loc(t, Root)

  let z_path (Loc (_, p)) = p

  let example =
    Branch (Branch (Leaf 1, Leaf 2), Branch (Leaf 3, Branch (Leaf 4, Branch (Leaf 5, Leaf 6))))

  let to_seq x =
    let start = x |> first in

    let rec f x =
      let x' = float_right x in
      let next () = Seq.Cons (x, match z_path x' with
        | Root -> fun () -> Seq.Nil
        | _ -> x' |> sink_left |> f
        )
      in next
    in

    f start

end

include Btree

let v = example |> z_of_tree |> to_seq |> List.of_seq
