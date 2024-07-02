let rec length = function [] -> 0 | _ :: t -> 1 + length t

type node = { value : int; next : mylist }
and mylist = [] | Node of node

let n = Node { value = 1; next = [] }
