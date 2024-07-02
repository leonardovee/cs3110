module type X = sig
  val x : int
end

module A : X = struct
  let x = 0
end

module IncX =
functor
  (M : X)
  ->
  struct
    let x = M.x + 1
  end
