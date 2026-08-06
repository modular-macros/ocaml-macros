(* TEST
 readonly_files = "macro_rec_env_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "macro_rec_env_lib.ml macro_rec_env_cross.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "macro_rec_env_lib.ml macro_rec_env_cross.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A [macro rec] group used from another module, where the members call
   each other *and* quote run-time bindings of their own module.  Each
   member's run-time slot holds the group's one shared environment, so a
   dependent reads either slot, and a sibling call inside the group passes
   that environment through unchanged -- under per-macro environments a
   sibling's slot was a local name, invalid here. *)

let v = $(Macro_rec_env_lib.a 3)
let w = $(Macro_rec_env_lib.b 2)

let () = Printf.printf "v = %d\nw = %d\n" v w
