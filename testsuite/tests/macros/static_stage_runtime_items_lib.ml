exception E of int
type t = ..
type t += A of int
module Make (X : sig val n : int end) = struct let v = X.n * 2 end
let f () = 7
