(* TEST
 readonly_files = "macro_env_hidden_lib.mli macro_env_hidden_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "macro_env_hidden_lib.mli macro_env_hidden_lib.ml macro_env_hidden.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "macro_env_hidden_lib.mli macro_env_hidden_lib.ml macro_env_hidden.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The case the whole mechanism exists for: the macro quotes [helper], which
   the signature does not export.  The environment captures the *value* of
   [helper], not its name, so whether the signature mentions it is
   irrelevant.  The two compiled blocks share a layout, so the restricting
   signature's coercion applies to both. *)

let z = $(Macro_env_hidden_lib.gen ())

let () = Printf.printf "z = %d\n" z
