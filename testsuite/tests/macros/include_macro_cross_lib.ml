(* Auxiliary module for include_macro_cross.ml.  Not a test in its own
   right. *)

(* A module whose only macro arrives by including a literal structure: the
   emission test for the macro object must look inside the include. *)

include struct
  let base = 100
  macro g () = << base + 8 >>
end
