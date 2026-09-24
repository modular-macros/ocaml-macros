(* TEST
 readonly_files = "splice_object_imports_helper.ml splice_object_imports_lib.ml";
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 all_modules = "\
   splice_object_imports_helper.ml \
   splice_object_imports_lib.ml \
   splice_object_imports.ml";
 ocamlc.byte;
 program = "splice_object_imports.cmo";
 ocamlobjinfo;
 check-program-output;
*)

(* A unit with a top-level splice has its object emitted by its static
   program, which has no typing phase and so no [Env.imports ()] of its
   own.  The import table below must still name every interface this unit
   depends on -- Splice_object_imports_helper above all, which the splice
   has nothing to do with.  An empty table is not cosmetic: it is what the
   bytecode linker checks inconsistent assumptions through, so a unit
   carrying one links happily against a stale dependency. *)

let x = $(Splice_object_imports_lib.zero ())
let y = Splice_object_imports_helper.hv
let () = ignore (x + y)
