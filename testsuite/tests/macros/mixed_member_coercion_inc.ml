(* include of a MACRO-BEARING unit under a narrowing mli: the coercion
   reaches run members inside a real macro block. *)
include Mixed_member_coercion_lib
let w = 3
let z = $(m ())
