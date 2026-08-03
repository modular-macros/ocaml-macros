(* The library side of static_program_ident_stamps.ml: a template functor
   with a purely run-time result, so that a client can force the
   static-program path with an application alone. *)

module F [X : sig val n : int end] = struct
  let v = X.n * 2
end
