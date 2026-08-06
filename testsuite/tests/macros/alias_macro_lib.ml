(* Auxiliary module for alias_macro.ml.  Not a test in its own right. *)

let helper x = x + 1

macro gen () = << helper 5 >>

(* A macro that quotes nothing.  Its environment is empty. *)
macro closed () = << 9 >>
