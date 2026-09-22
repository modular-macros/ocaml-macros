module type S = sig val v : int end

let run (p : (module S)) = let module X = (val p) in X.v + 1

let seven = 7
let id x = x
