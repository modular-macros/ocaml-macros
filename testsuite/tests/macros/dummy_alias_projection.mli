(* Listing [Infix] is the point: the struct's [Infix] is an alias, so
   the signature match delivers it through [Tcoerce_alias], whose path
   [App.Sub] roots at a compile-time dummy. *)

module Infix : sig val y : int end

macro m : int expr -> int expr

val w : int
