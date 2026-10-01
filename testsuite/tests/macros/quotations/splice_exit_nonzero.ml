(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Compile-time code that ends the compilation with a failing status:
   the status is reported, and the static program's object and object
   list are kept for inspection. *)
let a = $(exit 3)
