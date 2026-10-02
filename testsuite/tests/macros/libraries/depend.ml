(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 commandline = "-depend -slash -I-static lib -static-use counter.cmo \
                depend.ml";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/depend.reference";
 check-ocamlc.byte-output;
*)

(* ocamldep searches the directories given to -I-static. *)

let n = $(Expr.int (Counter.next ()))
