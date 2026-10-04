(* TEST
 readonly_files = "translation_lib.mli translation_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "translation_lib.mli translation_lib.ml";
   compile_only = "true";
   ocamlc.byte;
   all_modules = "translation_lib\$macros.cmo";
   compile_only = "false";
   program = "${test_build_directory}/compile_time_part.byte";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "translation_lib.mli translation_lib.ml";
   compile_only = "true";
   ocamlopt.byte;
   all_modules = "translation_lib\$macros.cmo";
   compile_only = "false";
   program = "${test_build_directory}/compile_time_part.byte";
   ocamlc.byte;
   run;
   check-program-output;
 }
*)

(* The compile-time part of a unit is a bytecode object of its own,
   whichever compiler writes it, which evaluates the static bindings of
   the unit and none of its run-time items.  This file is not
   compiled. *)
