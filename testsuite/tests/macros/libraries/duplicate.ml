(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml";
 compile_only = "true";
 ocamlc.byte;
 all_modules = "lib/counter.cmo";
 program = "lib/counters.cma";
 compile_only = "false";
 flags = "-a";
 ocamlc.byte;
 all_modules = "duplicate.ml";
 compile_only = "true";
 ocamlc_byte_exit_status = "2";
 {
   flags = "-I-static lib -static-use counter.cmo -static-use ./lib/counter.cmo";
   ocamlc.byte;
 }{
   flags = "-I-static lib -static-use counter.cmo -static-use counters.cma";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* Two files given to -static-use that define the same unit are
   refused, as is a file given twice. *)

let n = $(Expr.int (Counter.next ()))
