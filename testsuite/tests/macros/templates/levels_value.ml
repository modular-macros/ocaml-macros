(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A value of the parameter is a run-time value inside the body. *)

module type V = sig val v : int end
module F $(X : V) = struct let y = $(Expr.int X.v) end
