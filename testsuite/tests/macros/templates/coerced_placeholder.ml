(* TEST
 readonly_files = "coerced_placeholder_lib.ml coerced_placeholder_use.mli \
   coerced_placeholder_use.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "coerced_placeholder_lib.ml coerced_placeholder_use.mli \
     coerced_placeholder_use.ml coerced_placeholder.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "coerced_placeholder_lib.ml coerced_placeholder_use.mli \
     coerced_placeholder_use.ml coerced_placeholder.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A submodule with no compile-time part is a placeholder in the
   compile-time block of the module around it.  The interface reorders
   or hides its fields, and its sibling applies a template, so the
   compile-time block is coerced: the coercion keeps the placeholder
   rather than projecting its fields.  In [M] directly, and in [A.N] two
   levels down. *)

module U = Coerced_placeholder_use

let () =
  Printf.printf "%d %d %d %d %d\n"
    U.M.D.x U.M.D.y U.M.G.x U.A.N.D.y U.A.N.G.x
