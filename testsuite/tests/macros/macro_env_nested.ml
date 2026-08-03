(* TEST
 readonly_files = "macro_env_nested_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "macro_env_nested_lib.ml macro_env_nested.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "macro_env_nested_lib.ml macro_env_nested.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Calling a macro that lives in a nested module of another module.  The
   defining module has no top-level macro, so its macro object is only
   emitted if the emission test recurses into literal sub-structures. *)

let x = $(Macro_env_nested_lib.N.g ())

(* Two levels of nesting. *)
let y = $(Macro_env_nested_lib.Outer.Inner.h ())

let () = Printf.printf "x = %d\ny = %d\n" x y
