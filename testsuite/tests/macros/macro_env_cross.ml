(* TEST
 readonly_files = "macro_env_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "macro_env_lib.ml macro_env_cross.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "macro_env_lib.ml macro_env_cross.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Calling a macro of another module, where the code it returns names a
   run-time binding of *that* module.  The environment is reached as a field
   of the defining module's block, so the returned code needs nothing of
   this module's, and the compile-time world never loads the defining
   module's run-time object. *)

let z = $(Macro_env_lib.gen ())

(* A macro of another module whose callee is a macro of *its* module: the
   callee's environment rides in the caller's, so this works across the
   module boundary. *)
let w = $(Macro_env_lib.outer ())

(* An empty environment. *)
let c = $(Macro_env_lib.closed ())

(* A macro of this module calling a macro of another: no chaining is needed,
   since the callee's environment is an absolute reference either way. *)
macro here () = << 100 + $(Macro_env_lib.gen ()) >>

let h = $(here ())

let () = Printf.printf "z = %d\nw = %d\nc = %d\nh = %d\n" z w c h
