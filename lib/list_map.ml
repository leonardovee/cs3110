let add1' lst = List.map (fun x -> x + 1) lst
let concat' lst = List.map (fun x -> x ^ " 3110") lst
let lst1 = add1' [ 1; 2; 3 ]
let lst2 = concat' [ "a"; "b"; "c" ]
