(* Auxiliary module for macro_env_hidden.ml.  Not a test in its own right. *)

let helper x = x + 1

macro gen () = << helper 5 >>
