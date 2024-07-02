module type Set = sig
  (* ['a t] is the type of a set whose elements have a type ['a]. *)
  type 'a t

  (* [empty] is the empty set.  *)
  val empty : 'a t

  (* [size s] is the number of elements in [s].
        [size empty is [0]. *)
  val size : 'a t -> int

  (* [add x s] is a set containing all the elements of [s]
        as well as element [x]*)
  val add : 'a -> 'a t -> 'a t

  (* [mem x s] is [true] if [x] is an element os [s].  *)
  val mem : 'a -> 'a t -> bool

  (* [union s1 s2] is the set containing both the elements of
     [s1] and the elemtns of [s2]. *)
  val union : 'a t -> 'a t -> 'a t
end

module ListSet : Set = struct
  (* The list [a1; ...; an] represents the set {a1, ..., an},
     The empty list [[]] represents the empty set.
     The list must not contain duplicates. *)
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let size = List.length
  let add x s = if List.mem x s then s else x :: s
  let union s1 s2 = s1 @ s2 |> List.sort_uniq Stdlib.compare
end

module ListSetDups : Set = struct
  (* The list [a1; ...; an] represents the set {a1, ..., an},
     The empty list [[]] represents the empty set.
     The list may contain duplicates. *)
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let size s = s |> List.sort_uniq Stdlib.compare |> List.length
  let add x s = x :: s
  let union = List.append
end

module BstSet = struct
  type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (y, l, r) ->
        if x < y then mem x l else if x > y then mem x r else true

  let rec insert x = function
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (y, l, r) as t ->
        if x < y then Node (y, insert x l, r)
        else if x > y then Node (y, l, insert x r)
        else t
end

module RbSet = struct
  type color = Red | Black
  type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (_, v, l, r) ->
        if x < v then mem x l else if x > v then mem x r else true

  let balance = function
    | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
    | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
    | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
    | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)

  let insert x s =
    let rec ins = function
      | Leaf -> Node (Red, x, Leaf, Leaf)
      | Node (color, y, a, b) as s ->
          if x < y then balance (color, y, ins a, b)
          else if x > y then balance (color, y, a, ins b)
          else s
    in
    match ins s with
    | Node (_, y, a, b) -> Node (Black, y, a, b)
    | Leaf -> failwith "RBT insert failed with ins returning leaf"
end
