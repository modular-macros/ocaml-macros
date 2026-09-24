(* Auxiliary module for quote_module_root.ml.  Not a test in its own
   right. *)

let ( let* ) x f = f x
let ( and* ) x y = (x, y)

module M = struct let v = 5 end
module type S = sig val v : int end

(* Three ways a quotation reaches a run-time binding of this module
   without naming it as an identifier: a binding operator, whose
   [let*]/[and*] live in the binding_op record; [let module N = M],
   whose module expression is a path; and a first-class module pack.
   Each is a run-time name the $gen must be closure-converted over, so
   that a caller in another module reaches it through this module's
   block. *)
$gen letop () = << let* y = 3 in y + 1 >>

$gen letandop () = << let* y = 3 and* z = 4 in y + z >>

$gen letmodule () = << let module N = M in N.v >>

$gen pack () = << (module M : S) >>

(* A module bound INSIDE the quotation is not a run-time name of this
   module: its binder travels with the term, so [N] must not join the
   environment (and [M], which it aliases, must). *)
$gen letmodule_chain () = << let module N = M in let module P = N in P.v >>

(* A quotation naming the module through a value path as well, so the
   two routes to the same root agree on one environment slot. *)
$gen both () = << let module N = M in N.v + M.v >>
