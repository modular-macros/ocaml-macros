(* TEST
 readonly_files = "dummy_member_coercion_lib.ml dummy_member_coercion.mli";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "dummy_member_coercion_lib.ml dummy_member_coercion.mli \
                  dummy_member_coercion.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "dummy_member_coercion_lib.ml dummy_member_coercion.mli \
                  dummy_member_coercion.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* A restricting signature over a member the compile-time stage dummies.
   [include] of a macro-free named module gives [Sub] a dummy slot in
   the compile-time block (layout parity; see [Translmod.dummy_slots]),
   and the .mli's narrower [Sub] makes the block-build coercion
   structural.  Applied to the dummy, the coercion compiled to field
   reads of an immediate, and this unit's own static program -- the
   splice forces one -- crashed with SIGSEGV while building.  A dummy's
   coerced view is now the dummy itself. *)

include Dummy_member_coercion_lib

let w = $( << 3 >> )

let () = Printf.printf "%d %d\n" Sub.y w
