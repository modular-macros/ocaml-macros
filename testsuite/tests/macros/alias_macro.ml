(* TEST
 readonly_files = "alias_macro_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "alias_macro_lib.ml alias_macro.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "alias_macro_lib.ml alias_macro.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Calling a macro through a module alias.  At the compile-time stage the
   alias must resolve to the module's compile-time block -- the static
   program never links the run-time global (REVIEW.md B5).  The environment
   the returned code projects from stays in the run-time block, resolved at
   the final link. *)

module A = Alias_macro_lib

let x = $(A.gen ())
let c = $(A.closed ())

(* An alias of an alias resolves through, too. *)
module B = A

let y = $(B.gen ())

let () = Printf.printf "x = %d\nc = %d\ny = %d\n" x c y
