(* TEST
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An exception that a static binding raises is an error of the
   compilation of the unit. *)

$let x : int = failwith "no value"
let a = $(Expr.int x)
