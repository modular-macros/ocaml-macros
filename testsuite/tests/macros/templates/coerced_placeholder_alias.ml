(* TEST
 readonly_files = "coerced_placeholder_alias_lib.mli \
   coerced_placeholder_alias_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "coerced_placeholder_alias_lib.mli \
     coerced_placeholder_alias_lib.ml coerced_placeholder_alias.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "coerced_placeholder_alias_lib.mli \
     coerced_placeholder_alias_lib.ml coerced_placeholder_alias.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* An alias member, [module Mark = G0.Mark], whose target has no
   compile-time part, inside the result of a template application.  The
   interface hides a value of [Mark], so the compile-time part coerces
   the target of the alias, which is a placeholder there. *)

let () = print_int (Coerced_placeholder_alias_lib.T.Mark.get 0); print_newline ()
