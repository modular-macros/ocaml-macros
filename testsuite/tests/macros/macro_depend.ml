(* TEST
 subdirectories = "macro_depend_dir";
 setup-ocamlc.byte-build-env;
 commandline = "-depend -slash -I-static macro_depend_dir \
                -static-use macro_depend_dir/macro_depend_lib.cmo \
                macro_depend.ml";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/macro_depend.reference";
 check-ocamlc.byte-output;
*)

(* ocamldep rejected -I-static as an unknown option, so a macro project
   could not pass the compiler's own include flags to dependency
   generation and the level -1 imports were invisible to it.  A level -1
   use is an ordinary module reference in the source -- dependency
   extraction does not distinguish levels -- so a -I-static directory is
   searched alongside -I's.  -static-use names built archives rather than
   sources, so it is accepted and ignored, which is what lets one flag
   list serve both tools. *)

let x = $(Macro_depend_lib.zero ())
let () = ignore x
