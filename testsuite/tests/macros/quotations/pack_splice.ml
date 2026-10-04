(* TEST
 readonly_files = "pack_splice_main.ml";
 {
   setup-ocamlc.byte-build-env;
   {
     flags = "-for-pack Pack_splice_p";
     module = "pack_splice.ml";
     compile_only = "true";
     ocamlc.byte;
   }{
     program = "pack_splice_p.cmo";
     flags = "-pack";
     all_modules = "pack_splice.cmo";
     ocamlc.byte;
   }{
     program = "${test_build_directory}/packed.byte";
     flags = "";
     all_modules = "pack_splice_p.cmo pack_splice_main.ml";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }{
   setup-ocamlopt.byte-build-env;
   {
     flags = "-for-pack Pack_splice_p";
     module = "pack_splice.ml";
     compile_only = "true";
     ocamlopt.byte;
   }{
     program = "pack_splice_p.cmx";
     flags = "-pack";
     all_modules = "pack_splice.cmx";
     ocamlopt.byte;
   }{
     program = "${test_build_directory}/packed.opt";
     flags = "";
     all_modules = "pack_splice_p.cmx pack_splice_main.ml";
     ocamlopt.byte;
     run;
     check-program-output;
   }
 }
*)

(* A unit with a top-level splice and no compile-time component can be
   a member of a pack: its object is written for the pack by its static
   program. *)
let v = $( << 41 + 1 >> )
