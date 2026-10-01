(* TEST
 readonly_files = "macros_object_plain.ml pack_member.ml macros_object.sh";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "macros_object_plain.ml pack_member.ml";
 {
   ocamlc.byte;
   script = "sh macros_object.sh default";
   script;
 }{
   flags = "-macros-object auto";
   ocamlc.byte;
   script = "sh macros_object.sh auto";
   script;
 }{
   flags = "-macros-object always";
   ocamlc.byte;
   script = "sh macros_object.sh always";
   script;
 }{
   flags = "-macros-object none";
   ocamlc.byte;
   script = "sh macros_object.sh none";
   script;
   output = "macros_object.result";
   reference = "${test_source_directory}/macros_object.reference";
   check-program-output;
 }
*)

(* The option -macros-object says whether the compile-time part of a
   unit is written: by default, if the unit has compile-time components.
   This file is not compiled. *)
