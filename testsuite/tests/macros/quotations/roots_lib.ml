(* Auxiliary module for roots.ml.  Not a test in its own
   right. *)

let ( let* ) x f = f x
let ( and* ) x y = (x, y)

module M = struct let v = 5 end
module type S = sig val v : int end

$let[..] letop () = << let* y = 3 in y + 1 >>

$let[..] letandop () = << let* y = 3 and* z = 4 in y + z >>

$let[..] letmodule () = << let module N = M in N.v >>

$let[..] pack () = << (module M : S) >>

(* A module bound inside the quotation is not a run-time name. *)
$let[..] letmodule_chain () = << let module N = M in let module P = N in P.v >>

$let[..] both () = << let module N = M in N.v + M.v >>
