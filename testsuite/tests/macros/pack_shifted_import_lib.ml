module type S = sig val v : int end

let consume (p : (module S)) = let module X = (val p) in X.v + 1

module V = struct let v = 41 end
