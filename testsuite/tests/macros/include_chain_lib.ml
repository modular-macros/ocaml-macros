(* Auxiliary module for include_chain.ml.  Not a test in its own right. *)

let base = 40
let helper x = x + 2

$gen gen () = << helper base >>

$gen closed () = << 7 >>
