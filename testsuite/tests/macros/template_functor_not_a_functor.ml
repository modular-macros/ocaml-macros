(* TEST
 flags = "-stop-after typing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* There is no subtyping relation between the two kinds of functor.
   The other direction, an ordinary functor where a template functor is
   expected, is checked in template_functor_typing.ml. *)

module type S = sig type t end
module T[X : S] = struct type t = X.t end

module Bad : (X : S) -> S = T
