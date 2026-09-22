(* Auxiliary module for toplevel_twin_dummy_coercion.ml.  Not a test in
   its own right.  The include brings [Sub] in as a compile-time dummy;
   the .mli's narrower [Sub] makes the macros object's block-build
   coercion structural. *)

include Dummy_member_coercion_lib

let w = 3
