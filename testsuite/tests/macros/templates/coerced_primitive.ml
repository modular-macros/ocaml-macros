(* TEST
 readonly_files = "coerced_primitive_stub.c coerced_primitive_lib.mli \
   coerced_primitive_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "coerced_primitive_stub.c coerced_primitive_lib.mli \
     coerced_primitive_lib.ml coerced_primitive.ml";
   flags = "-custom";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "coerced_primitive_stub.c coerced_primitive_lib.mli \
     coerced_primitive_lib.ml coerced_primitive.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* An external, [succ], exported as a value, beside a template
   application.  The interface coerces [succ] to a closure around the
   primitive; in the compile-time block, whose static program has no C
   stubs, its slot is the placeholder. *)

let () = Printf.printf "%d\n" (Coerced_primitive_lib.succ 41)
