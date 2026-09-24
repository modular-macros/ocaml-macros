(* TEST
 flags = "-stop-after typing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A template functor must be applied with angle brackets.  The
   converse, applying an ordinary functor as F[M], is checked in
   template_functor_typing.ml, which does not need a template functor
   value and so can be an expect test. *)

module type S = sig type t end
module T[X : S] = struct type t = X.t end

module Bad = T(struct type t = int end)
