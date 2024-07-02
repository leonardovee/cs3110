(* Mutable singly-linked lists *)

(* An ['a node] is a node of mutable singl  linked list.
   It containes a value of type ['a] and optionally has
   a pointer to the next node.*)
type 'a node = { value : 'a; mutable next : 'a node option }

(* An ['a mlist] is a mutable singly-linked list with
   elements of type ['a]. *)
type 'a mlist = { mutable first : 'a node option }

(* [create_node v] is a node containing value [v]
   with no link to another node.*)
let create_node v = { next = None; value = v }

(* [singleton v] is a singly-linked list containing
   exactly one value, [v]. *)
let singleton v = { first = Some (create_node v) }

(* [insert_first lst n] mutates [lst] by inserting value
   [v] as the first value in the list.*)
let insert_first lst v =
  match lst.first with
  | None -> lst.first <- Some (create_node v)
  | was_first ->
      let new_first = create_node v in
      new_first.next <- was_first;
      lst.first <- Some new_first

let lst = singleton 3110
let empty () = { first = None }
