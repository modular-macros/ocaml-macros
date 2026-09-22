module type S = sig val v : int end

let run (p : (module S)) = let module X = (val p) in X.v + 1

module type Nested = sig module Sub : sig val w : int end end

let run_nested (p : (module Nested)) =
  let module X = (val p) in X.Sub.w * 2
