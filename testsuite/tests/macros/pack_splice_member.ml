(* TEST
 readonly_files = "pack_splice_member_main.ml";
 {
   setup-ocamlc.byte-build-env;
   {
     flags = "-for-pack Pack_splice_member_p";
     module = "pack_splice_member.ml";
     compile_only = "true";
     ocamlc.byte;
   }{
     program = "pack_splice_member_p.cmo";
     flags = "-pack";
     all_modules = "pack_splice_member.cmo";
     ocamlc.byte;
   }{
     program = "${test_build_directory}/packed.byte";
     flags = "";
     all_modules = "pack_splice_member_p.cmo pack_splice_member_main.ml";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }{
   setup-ocamlopt.byte-build-env;
   {
     flags = "-for-pack Pack_splice_member_p";
     module = "pack_splice_member.ml";
     compile_only = "true";
     ocamlopt.byte;
   }{
     program = "pack_splice_member_p.cmx";
     flags = "-pack";
     all_modules = "pack_splice_member.cmx";
     ocamlopt.byte;
   }{
     program = "${test_build_directory}/packed.opt";
     flags = "";
     all_modules = "pack_splice_member_p.cmx pack_splice_member_main.ml";
     ocamlopt.byte;
     run;
     check-program-output;
   }
 }
*)

(* This file is a pack MEMBER: it has a top-level splice and nothing at
   level -1 that outlives its own compilation.  -pack refuses a member
   whose signature has level -1 components (pack_macro_member.ml and its
   native twin), because the member's compile-time object cannot be
   renamed into the pack; a splice-bearing member is the other case and
   stays packable (LIMITATIONS.md, pack-macro-members).  Its compile-time
   part is the static program that WRITES its object, and that program
   runs during this compilation, leaving the pack nothing to carry.

   What the object must record is -for-pack itself.  It is written by
   that separate process, out of reach of the parent's Clflags
   (splice_object_flags.ml pins the channel the flags travel down), and
   an object written without the flag is refused by the NATIVE pack step,
   which reads the recorded name back: "was not compiled with the
   -for-pack Pack_splice_member_p option".  So the native branch above is
   what fails if the flag is ever dropped again.  The member is packed,
   the pack linked, and the value read back through the pack's name by
   pack_splice_member_main.ml. *)

let v = $( << 40 >> ) + 2

let () = print_endline "member initialised"
