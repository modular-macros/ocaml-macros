(* TEST
 readonly_files = "macros_object_none_lib.ml";
 setup-ocamlopt.byte-build-env;
 compile_only = "true";
 flags = "-macros-object none";
 all_modules = "macros_object_none_lib.ml";
 ocamlopt.byte;
 flags = "";
 all_modules = "macros_object_none.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* With -macros-object none, the native compiler does not write the
   compile-time part of a unit: the unit's own splices still use its
   static bindings and generators, and a unit that uses them reports the
   missing object. *)

let x = $(Macros_object_none_lib.g ())
