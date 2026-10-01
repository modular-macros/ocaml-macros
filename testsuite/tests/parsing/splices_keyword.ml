(* TEST
 flags = "-stop-after parsing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A dollar glued to a keyword with no staged form is an error. *)

let a = << $if >>
