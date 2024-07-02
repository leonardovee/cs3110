module type Queue = sig
  type 'a queue

  val empty : 'a queue
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a option
  val dequeue : 'a queue -> 'a queue option
end

module ListQueue : Queue = struct
  type 'a queue = 'a list

  let empty = []
  let enqueue x q = q @ [ x ]
  let peek = function [] -> None | x :: _ -> Some x
  let dequeue = function [] -> None | _ :: q -> Some q
end

module TwoListQueue = struct
  (* [{front = [a; b]; back = [e; d; c]}]
     represents the queue a,b,c,d,e
     if [front] is empty then [back] must also be empty
  *)
  type 'a queue = { front : 'a list; back : 'a list }

  let empty = { front = []; back = [] }

  let peek = function
    | { front = []; _ } -> None
    | { front = x :: _; _ } -> Some x

  let enqueue x = function
    | { front = []; _ } -> { front = [ x ]; back = [] }
    | q -> { q with back = x :: q.back }

  let dequeue = function
    | { front = []; _ } -> None
    | { front = _ :: []; back } -> Some { front = List.rev back; back = [] }
    | { front = _ :: t; back } -> Some { front = t; back }
end
