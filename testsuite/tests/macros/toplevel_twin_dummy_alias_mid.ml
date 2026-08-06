(* Auxiliary module for toplevel_twin_dummy_alias.ml.  Not a test in
   its own right.  [App] is a macro-free plain functor application, a
   compile-time dummy; the .mli lists [Infix], so the macros object's
   block-build coercion translates the alias path [App.Sub] through
   the dummy. *)

module F (X : sig val x : int end) = struct
  module Sub = struct let y = X.x end
end

module App = F (struct let x = 1 end)

module Infix = App.Sub

macro m : int expr -> int expr = fun x -> x
