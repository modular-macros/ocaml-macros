(* TEST
 readonly_files = "dummy_member_coercion_lib.ml \
                   toplevel_twin_dummy_coercion_mid.mli \
                   toplevel_twin_dummy_coercion_mid.ml";
 setup-ocamlc.byte-build-env;
 flags = "-macros-object always";
 all_modules = "dummy_member_coercion_lib.ml \
                toplevel_twin_dummy_coercion_mid.mli \
                toplevel_twin_dummy_coercion_mid.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* The consumer side of dummy_member_coercion: under [-macros-object
   always] -- how the staged library ports build -- every unit gets a
   $macros twin, macros or not, and #load probes for a loaded unit's
   twin by name and RUNS it.  The mid unit's twin carried the
   block-build coercion of its restricting .mli applied to the dummied
   include member: field reads of an immediate, SIGSEGV at #load with
   the toplevel's own frame on the stack (how the staged ctypes pin
   took the session down).  A dummy's coerced view is now the dummy,
   and the twin loads inertly. *)

#load "dummy_member_coercion_lib.cmo";;

#load "toplevel_twin_dummy_coercion_mid.cmo";;

let a = Toplevel_twin_dummy_coercion_mid.Sub.y;;
let b = Toplevel_twin_dummy_coercion_mid.w;;
let () = Printf.printf "%d\n" (a + b);;
