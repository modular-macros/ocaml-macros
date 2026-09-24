(* Auxiliary module for include_named.ml.  Not a test in its own right. *)

let helper x = x + 1

$gen gen () = << helper 5 >>

(* A $gen that quotes nothing.  Its environment is empty. *)
$gen closed () = << 9 >>
