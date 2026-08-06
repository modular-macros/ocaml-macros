(* TEST
 readonly_files = "include_named_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "include_named_lib.ml include_named.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "include_named_lib.ml include_named.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Macros arriving via [include] of a *named* module (D-auto-5): at the
   compile-time stage the include is translated off the module's macro
   block, whose layout the run-time block shares, so the macros are bound
   for real -- their functions from that block, values its dummies.  [gen]
   quotes a run-time binding of the library, reached through the environment
   in the run-time slot the include re-exports. *)

include Include_named_lib

let y = $(gen ())
let c = $(closed ())

let () = Printf.printf "y = %d\nc = %d\n" y c
