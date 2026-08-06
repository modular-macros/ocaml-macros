(* Auxiliary module for quote_pack_macro.ml.  Not a test in its own
   right.  It has a macro component, so it has a compile-time block. *)

let v = 5

macro one () = << 1 >>
