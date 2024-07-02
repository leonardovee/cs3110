module type Fact = sig
  (* [fact n] is [n] factorial. *)
  val fact : int -> int
end

module RecursiveFact : Fact = struct
  let rec fact n = if n = 0 then 1 else n * fact (n - 1)
end
