(* TEST
 readonly_files = "pack_macro_member_a.ml";
 setup-ocamlopt.byte-build-env;
 {
   flags = "-for-pack Pack_macro_member_p";
   module = "pack_macro_member_a.ml";
   compile_only = "true";
   ocamlopt.byte;
 }{
   program = "pack_macro_member_p.cmx";
   flags = "-pack";
   all_modules = "pack_macro_member_a.cmx";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* The native twin of pack_macro_member.ml.  The refusal is written out
   twice, in bytepackager and in asmpackager, with the same text and the
   same has_macro_components predicate behind it; the compile-time world
   is bytecode in both back ends (LIMITATIONS.md, bytecode-only-macros),
   so a native pack has exactly the same missing p$macros.cmo to answer
   for.  Two copies of a message are two places to change, so each needs
   its own test: no search of the expected outputs can tell which of them
   a given .reference came from.  This file is never compiled. *)
