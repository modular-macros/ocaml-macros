(* TEST
 readonly_files = "pack_member.ml";
 {
   setup-ocamlc.byte-build-env;
   flags = "-for-pack Pack_p";
   module = "pack_member.ml";
   compile_only = "true";
   ocamlc.byte;
   compile_only = "false";
   program = "pack_p.cmo";
   flags = "-pack";
   all_modules = "pack_member.cmo";
   module = "";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   setup-ocamlopt.byte-build-env;
   flags = "-for-pack Pack_p";
   module = "pack_member.ml";
   compile_only = "true";
   ocamlopt.byte;
   compile_only = "false";
   program = "pack_p.cmx";
   flags = "-pack";
   all_modules = "pack_member.cmx";
   module = "";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   compiler_reference = "${test_source_directory}/pack.opt.reference";
   check-ocamlopt.byte-output;
 }
*)

(* The compile-time part of a unit is not packed, so a unit that has
   one cannot be a member of a pack.  This file is not compiled. *)
