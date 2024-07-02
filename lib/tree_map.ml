type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))

let rec map f = function
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map f l, map f r)

let add1 t = map succ t
