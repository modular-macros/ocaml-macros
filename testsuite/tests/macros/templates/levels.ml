(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A generator of the parameter is a generator inside the body. *)

module type G = sig $val[..] m : int expr -> int expr end
module F $(X : G) = struct let y = X.m << 3 >> end
