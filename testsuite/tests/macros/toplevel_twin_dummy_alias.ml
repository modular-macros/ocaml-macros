(* TEST
 readonly_files = "toplevel_twin_dummy_alias_mid.mli \
                   toplevel_twin_dummy_alias_mid.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "toplevel_twin_dummy_alias_mid.mli \
                toplevel_twin_dummy_alias_mid.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* The consumer side of dummy_alias_projection: the mid unit carries a
   macro, so it gets a $macros twin, and #load probes for a loaded
   unit's twin by name and RUNS it.  The twin carried the block-build
   coercion's [Tcoerce_alias] translation of [App.Sub] -- a field read
   of the dummied [App], an immediate -- and running it took the
   session down with SIGSEGV (how the fast_gen pin's [Bq_generator]
   twin took out `#require "fast_gen"`).  A path rooted at a dummy now
   translates to the dummy, the twin loads inertly, and the macro it
   carries is callable from a later phrase's splice. *)

#load "toplevel_twin_dummy_alias_mid.cmo";;

let a = Toplevel_twin_dummy_alias_mid.Infix.y;;
let v = $(Toplevel_twin_dummy_alias_mid.m << 21 >>);;
let () = Printf.printf "%d\n" (a + v);;
