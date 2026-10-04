(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The two kinds of functor are unrelated as types. *)

module type S = sig type t end
module T $(X : S) = struct type t = X.t end
module Bad : (X : S) -> S = T
