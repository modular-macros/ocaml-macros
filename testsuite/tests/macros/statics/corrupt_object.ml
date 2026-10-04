(* TEST
 readonly_files = "missing_object_lib.ml corrupt_object.truncated";
 setup-ocamlc.byte-build-env;
 module = "missing_object_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 script = "cp corrupt_object.truncated missing_object_lib\$macros.cmo";
 script;
 module = "corrupt_object.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An object that is shorter than its magic number, as an interrupted
   build leaves, is reported as any file that is not an object is. *)

let a = $(Expr.int Missing_object_lib.x)
