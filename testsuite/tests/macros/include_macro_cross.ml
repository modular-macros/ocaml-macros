(* TEST
 readonly_files = "include_macro_cross_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "include_macro_cross_lib.ml include_macro_cross.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "include_macro_cross_lib.ml include_macro_cross.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Calling a macro that another module gained by [include struct ... end].
   The defining module has no top-level macro, so its macro object is only
   emitted if the emission test looks inside the include; the macro also
   quotes a run-time binding of the included structure, so its environment
   is reached through the defining module's block. *)

let x = $(Include_macro_cross_lib.g ())

let () = Printf.printf "x = %d\n" x
