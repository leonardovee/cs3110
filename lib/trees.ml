module Tree = struct
  type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

  let rec sum = function Leaf -> 0 | Node (v, l, r) -> v + sum l + sum r
end

let t : int Tree.tree =
  Node
    ( 4,
      Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)),
      Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) )

let lst = Tree.sum t
