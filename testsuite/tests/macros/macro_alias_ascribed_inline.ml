(* The inline route: the item is an alias, the delivered signature a
   macro-bearing one.  The signature also NARROWS (drops [v]) and
   REORDERS ([k] before [m]), so the coercion genuinely reshapes both
   the run-time and the compile-time block. *)
module R : sig
  macro k : int expr -> int expr
  macro m : unit -> int expr
end = Macro_alias_ascribed_lib.Inner
