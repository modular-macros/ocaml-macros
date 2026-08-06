(* TEST
 readonly_files = "\
   static_manifest_zz.ml static_manifest_aa.ml static_manifest_mac.ml";
 setup-ocamlc.byte-build-env;
 {
   all_modules = "static_manifest_zz.ml static_manifest_aa.ml";
   compile_only = "true";
   ocamlc.byte;
 }{
   all_modules = "static_manifest_mac.ml";
   flags = "-I-static . -static-use static_manifest_zz.cmo \
            -static-use static_manifest_aa.cmo";
   compile_only = "true";
   ocamlc.byte;
 }{
   all_modules = "static_manifest.ml";
   flags = "";
   compile_only = "true";
   ocamlc.byte;
   program = "static_manifest_mac\$macros.cmo";
   ocamlobjinfo;
   check-program-output;
 }
*)

(* A macro object records the -static-use archives its own compilation
   linked, so that a dependent's static program can link them too.  The
   list was deduplicated with a sort, which replaced the accumulation
   order -- a link order, dependencies first -- with an alphabetical one.
   Here Aa depends on Zz, so the two orders are opposites, and the sorted
   manifest made this file fail to link with "Zz referenced from Aa (is a
   -static-use archive missing?)" though nothing was missing.

   Compiling this file at all is the test; the objinfo below pins the
   recorded order, which was invisible until "Static archives:" was
   added. *)

let x = $(Static_manifest_mac.getw ())
let () = ignore x
