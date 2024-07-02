module type Map = sig
  (* [t] is the type of maps that bind keys to values. *)
  type ('k, 'v) t

  (* [insert k v m] is the same map as [m], but with an
     additional binding from [k] to [v]. If [k] was
     already bound in [m], that binding is replaced by the
     binding to [v] in the new map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (* [find k m] is [Some v] if [k] is bound to [v] in [m],
     and [None] if not. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (* [remove k m] is the same map as [m], but without any
     binding of [k]. If [k] was not bound in [m], then the map
     is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

  (* [empty] is the empty map. *)
  val empty : ('k, 'v) t

  (* [of_list lst] is a map containing the same bindings
     as association list [lst]. *)
  val of_list : ('k * 'v) list -> ('k, 'v) t

  (* [bindings m] is an association list containint the same
     bindings as [m]. There are no duplicate keys in the list. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module AssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list

  let insert k v m = (k, v) :: m
  let find = List.assoc_opt
  let remove k m = List.filter (fun (k', _) -> k <> k') m
  let empty = []
  let of_list lst = lst
  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare
  let binding m k = (k, List.assoc k m)
  let bindings m = List.map (binding m) (keys m)
end

module type DirectAddressMap = sig
  (* [t] is the type of maps that bind keys of type int to values of
      type ['v]. *)
  type 'v t

  (* [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to
      [v] in the new map. Requires: [k] is in bounds for [m]. *)
  val insert : int -> 'v -> 'v t -> unit

  (* [find k m] is [Some v] if [k] is bound to [v] in [m], and [None]
      if not. Requires: [k] is in bounds for [m]. *)
  val find : int -> 'v t -> 'v option

  (* [remove k m] mutates [m] to remove any binding of [k]. If [k] was
      not bound in [m], then the map is unchanged. Requires: [k] is in
      bounds for [m]. *)
  val remove : int -> 'v t -> unit

  (* [create c] creates a map with capacity [c]. Keys [0] through [c-1]
      are _in bounds_ for the map. *)
  val create : int -> 'v t
  val of_list : int -> (int * 'v) list -> 'v t
  (* [of_list c lst] is a map containing the same bindings as
      association list [lst] and with capacity [c]. Requires: [lst] does
      not contain any duplicate keys, and every key in [lst] is in
      bounds for capacity [c]. *)

  val bindings : 'v t -> (int * 'v) list
  (* [bindings m] is an association list containing the same bindings
      as [m]. There are no duplicate keys in the list. *)
end

module ArrayMap : DirectAddressMap = struct
  type 'v t = 'v option array

  let insert k v a = a.(k) <- Some v
  let find k a = a.(k)
  let remove k a = a.(k) <- None
  let create c = Array.make c (assert false)

  let of_list c lst =
    let a = create c in
    List.iter (fun (k, v) -> insert k v a) lst;
    a

  let bindings a =
    let b = ref [] in
    for k = 0 to Array.length a do
      match a.(k) with None -> () | Some v -> b := (k, v) :: !b
    done;
    !b
end

module type TableMap = sig
  (* [('k, 'v) t] is the type of mutable table-based maps that bind
      keys of type ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (* [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to
      [v]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit

  (* [find k m] is [Some v] if [m] binds [k] to [v], and [None] if [m]
      does not bind [k]. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (* [remove k m] mutates [m] to remove any binding of [k]. If [k] was
      not bound in [m], the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit

  (* [create hash c] creates a new table map with capacity [c] that
      will use [hash] as the function to convert keys to integers.
      Requires: The output of [hash] is always non-negative, and [hash]
      runs in constant time. *)
  val create : ('k -> int) -> int -> ('k, 'v) t
end

module HashMap : TableMap = struct
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k * 'v) list array;
  }

  (* [capacity tab] is the number of buckets in [tab].  *)
  let capacity tab = Array.length tab.buckets

  (* [index k tab] is the index at which key [k] should
     be stored in the buckets of [tab]. *)
  let index k tab = tab.hash k mod capacity tab

  (* [insert_no_resize k v tab] inserts a binding from [k] to
     [v] in [tab] and does no resize the table, regardless
     of what happens to the load factor. *)
  let insert_no_resize k v tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- (k, v) :: List.remove_assoc k old_bucket;
    if not (List.mem_assoc k old_bucket) then tab.size <- tab.size + 1;
    ()

  (* [load_factor tab] is the load factor of [tab].  *)
  let load_factor tab = float_of_int tab.size /. float_of_int (capacity tab)

  (* [rehash tab new_capacity] replaces the buckets array of [tab]
     with a new array of size [new_capacity], and re-inserts all
     of the bindings of [tab] into the new array. The keys are re-hashed
     so the bindings will go on new buckets. *)
  let rehash tab new_capacity =
    let rehash_binding (k, v) = insert_no_resize k v tab in
    let rehash_bucket b = List.iter rehash_binding b in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.make new_capacity [];
    tab.size <- 0;
    Array.iter rehash_bucket old_buckets

  (* [resize_if_needed tab] resizes and rehashes [tab] if the load
     factor is too big or too small.*)
  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf > 2.0 then rehash tab (capacity tab * 2)
    else if lf < 0.5 then rehash tab (capacity tab / 2)
    else ()

  let insert k v tab =
    insert_no_resize k v tab;
    resize_if_needed tab

  let find k tab = List.assoc_opt k tab.buckets.(index k tab)

  (* [remove_no_resize k tab] removes [k] from [tab] and does not
     trigger a resize, regardless of what happens to the load factor. *)
  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
    if List.mem_assoc k old_bucket then tab.size <- tab.size - 1;
    ()

  let remove k tab =
    remove_no_resize k tab;
    resize_if_needed tab

  let create h c = { hash = h; size = 0; buckets = Array.make c [] }
end
