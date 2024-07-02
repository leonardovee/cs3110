let rec sum = function [] -> 0 | h :: t -> h + sum t
let rec concat = function [] -> "" | h :: t -> h ^ concat t

let rec combine init op = function
  | [] -> init
  | h :: t -> op h (combine init op t)

let sum' lst = combine 0 ( + ) lst
let concat' lst = combine "" ( ^ ) lst

let rec fold_right f lst acc =
  match lst with [] -> acc | h :: t -> f h (fold_right f t acc)

let rec fold_left f acc lst =
  match lst with
  | [] -> acc
  | h :: t ->
      let acc' = f acc h in
      fold_left f acc' t

let lst1 = List.fold_right ( ^ ) [ "" ]
