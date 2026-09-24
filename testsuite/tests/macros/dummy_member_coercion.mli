(* The restriction is the point: delivering [Sub] under a narrower
   signature makes the coercion read fields out of the member, which at
   the compile-time stage is a dummy. *)

module Sub : sig val y : int end
val w : int
