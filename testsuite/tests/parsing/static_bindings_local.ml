(* TEST
 flags = "-stop-after parsing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A static binding is a structure item, not an expression. *)

let a = $let b = 1 in b
