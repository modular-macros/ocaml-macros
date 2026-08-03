(* TEST
 readonly_files = "static_import_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "static_import_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "static_import.ml";
   flags = "-I-static . -static-use static_import_lib.cmo";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "static_import_lib.cmo static_import.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* Case CR of SHIFTED-IMPORTS.md, in tree: the working directory is
   run-visible by default and -I-static adds the compile world, so
   the library serves both levels; -static-use links it into this
   unit's static program.  (The C/R denial matrix lives in the
   standalone tests repository, which controls directories freely.) *)

macro m () = Expr.int (Static_import_lib.double 10)

let both = $(m ()) + Static_import_lib.double 100

let () = Printf.printf "CR: %d %s\n" both Static_import_lib.greet
