(* Auxiliary module for include_chain.ml.  Not a test in its own right.

   A module whose macros all arrive by including a named module: it must
   still emit a $gen object, whose block re-exports the library's macros
   and whose relocations name the library's $gen object -- the transitive
   link the static program of include_chain.ml follows. *)

include Include_chain_lib
