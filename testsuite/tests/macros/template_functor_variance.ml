(* TEST
 flags = "-stop-after typing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The body of a template functor is covariant, so it may be narrowed
   but not widened.  The accepting directions, and the contravariance of
   the parameter, are in template_functor_accepted.ml. *)

module type S = sig type t end
module type BIG = sig val v : int val w : int end
module type SMALL = sig val v : int end

module Narrow : [X : S] SMALL = functor [X : S] -> struct let v = 1 end

module Bad : [X : S] BIG = Narrow
