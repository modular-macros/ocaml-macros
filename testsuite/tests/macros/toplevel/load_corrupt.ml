(* TEST
 readonly_files = "load_lib.ml load_corrupt.truncated";
 setup-ocamlc.byte-build-env;
 module = "load_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 script = "cp load_corrupt.truncated load_lib\$macros.cmo";
 script;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* An object that is shorter than its magic number, as an interrupted
   build leaves, is reported, and the session goes on. *)

#load "load_lib.cmo";;
let a = 1;;
