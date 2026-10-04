(* TEST
 readonly_files = "missing_object_lib.ml";
 setup-ocamlc.byte-build-env;
 module = "missing_object_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 script = "rm missing_object_lib\$macros.cmo";
 script;
 module = "missing_object.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The compile-time part of a unit is in an object of its own, which the
   interface of the unit does not replace. *)

let a = $(Expr.int Missing_object_lib.x)
