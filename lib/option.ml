let rec list_max lst =
  match lst with
  | [] -> None
  | h :: t -> (
      match list_max t with None -> Some h | Some m -> Some (max h m))

let _ = list_max [ 1; 2; 3 ]
