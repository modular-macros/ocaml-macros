(* The library side of static_program_exit_numbers.ml: a template functor
   with a purely run-time result, so that a client can force the
   static-program path with an application alone -- no macro of its own,
   hence no macro object, hence a run-time part translated with the
   parent's static-exception counter still at zero. *)

module F [X : sig val n : int end] = struct
  let v = X.n * 2
end
