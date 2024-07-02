module MyStack = struct
  type 'a t = Empty | Entry of 'a * 'a t

  let empty = Empty
  let push x s = Entry (x, s)
  let peek = function Empty -> failwith "Empty" | Entry (x, _) -> x
  let pop = function Empty -> failwith "Empty" | Entry (_, s) -> s
end

module ListStack = struct
  type 'a t

  let empty = []
  let push x s = x :: s
  let peek = function [] -> failwith "Empty" | x :: _ -> x
  let pop = function [] -> failwith "Empty" | _ :: s -> s
end

let p = ListStack.(empty |> push 1 |> push 2 |> push 3)

open ListStack

let v = empty |> push 1 |> push 2 |> push 3

open MyStack

let y = empty |> push 1 |> push 2 |> push 3
let x = empty |> push "a" |> push "b" |> push "c"
