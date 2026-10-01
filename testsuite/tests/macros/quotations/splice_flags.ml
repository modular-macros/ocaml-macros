(* TEST
 readonly_files = "splice_flags_lib.ml";
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 flags = "-linkall";
 all_modules = "\
   splice_flags_lib.ml \
   splice_flags.ml";
 ocamlc.byte;
 program = "splice_flags.cmo";
 ocamlobjinfo;
 check-program-output;
*)

(* The object of a unit with a top-level splice is written as that of
   any unit, with the flags of the compilation (-linkall, below) and
   its table of primitives. *)
external ident : int -> int = "%identity"
let x = $(Splice_flags_lib.zero ())
let () = ignore (ident x)
