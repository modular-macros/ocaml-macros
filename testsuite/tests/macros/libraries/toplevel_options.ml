(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I lib -I-static lib -static-use counter.cmo lib/counter.cmo";
 ocaml;
 check-ocaml-output;
*)

(* The options -I-static and -static-use of the toplevel do what the
   directives #static_dir and #static_use do.  Here the unit is loaded
   for run-time code too. *)

$let a = Counter.next ();;
let b = $(Expr.int (a + Counter.next ()));;
Counter.next ();;
