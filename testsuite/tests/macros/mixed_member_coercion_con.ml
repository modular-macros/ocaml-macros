(* the constrained-path form: the coercion is applied over the macro
   block's global directly, narrowing run members at three depths and
   a functor, while keeping the macro. *)
module X : sig
  module Sub : sig val y : int end
  module Outer : sig module Mid : sig module Deep : sig val q : int end end end
  module F : (Y : sig val v : int end) -> sig val h : int end
  macro m : unit -> int expr
end = Mixed_member_coercion_lib
let z = $(X.m ())
