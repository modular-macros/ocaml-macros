(* TEST
 readonly_files = "pack_macro_member_a.ml";
 setup-ocamlc.byte-build-env;
 {
   flags = "-for-pack Pack_macro_member_p";
   module = "pack_macro_member_a.ml";
   compile_only = "true";
   ocamlc.byte;
 }{
   program = "pack_macro_member_p.cmo";
   flags = "-pack";
   all_modules = "pack_macro_member_a.cmo";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* -pack copies run-time objects only, so a member's compile-time object
   (a$macros.cmo) is left out of the pack; the pack's interface then
   advertises macros no consumer can resolve, and the diagnosis lands in
   a different project as "cannot find p$macros.cmo".  The packer refuses
   the member instead (LIMITATIONS.md, pack-macro-members), located at the
   member and using the same has_macro_components predicate the drivers
   use to decide whether a unit needs a macro object at all.

   The member here has a macro and no splice, which is the case that used
   to pack "successfully".  This file is never compiled. *)
