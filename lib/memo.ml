let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let fib' n =
  let memo : int option array = Array.make (n + 1) None in
  let rec f_mem n =
    match memo.(n) with
    | Some result -> result
    | None ->
        let result = if n < 2 then 1 else f_mem (n - 1) + f_mem (n - 2) in
        memo.(n) <- Some result;
        result
  in
  f_mem n
