(* Auxiliary module for include_chain.ml.  Not a test in its own right. *)

let base = 40
let helper x = x + 2

macro gen () = << helper base >>

macro closed () = << 7 >>
