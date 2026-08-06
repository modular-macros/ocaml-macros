(* TEST
 readonly_files = "toplevel_include_submodule_lib.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "toplevel_include_submodule_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* [include] of a compiled unit whose macros live in a SUBMODULE.  The
   include decided how to rebind by counting the signature's direct macro
   values, found none, and copied each component as a plain run-time field:
   the submodule's table entry then held the batch run-time block, whose
   macro slots hold environment tuples, and the first use of one
   dereferenced a tuple as a table pair -- SIGSEGV, no diagnostic.  A module
   component carrying compile-time content is now rebound as the toplevel
   binds any such module: run block under its name, compile block under its
   record key. *)

#load "toplevel_include_submodule_lib.cmo";;

include Toplevel_include_submodule_lib;;

(* The macro one level down, and the direct one, from the same include. *)
let x = $(N.m ());;
let y = $(g ());;
let () = Printf.printf "%d\n" (x + y);;
