(* TEST
 flags = "-stop-after parsing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A dollar followed by a space is an operator, not a splice. *)

let a = $ x
