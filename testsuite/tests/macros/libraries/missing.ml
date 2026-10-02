(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "lib/counter.ml";
 ocamlc.byte;
 all_modules = "missing.ml";
 ocamlc_byte_exit_status = "2";
 {
   flags = "-I-static lib";
   ocamlc.byte;
 }{
   flags = "-I-static lib -static-use absent.cma";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* The implementation of a unit that compile-time code uses must be
   given to -static-use. *)

let n = $(Expr.int (Counter.next ()))
