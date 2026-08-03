(* TEST
 readonly_files = "macro_alias_ascribed_lib.ml macro_alias_ascribed_inline.ml \
   macro_alias_ascribed_mli.mli macro_alias_ascribed_mli.ml \
   macro_alias_ascribed_bare.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "macro_alias_ascribed_lib.ml macro_alias_ascribed_inline.ml \
     macro_alias_ascribed_mli.mli macro_alias_ascribed_mli.ml \
     macro_alias_ascribed_bare.ml macro_alias_ascribed.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "macro_alias_ascribed_lib.ml macro_alias_ascribed_inline.ml \
     macro_alias_ascribed_mli.mli macro_alias_ascribed_mli.ml \
     macro_alias_ascribed_bare.ml macro_alias_ascribed.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* An OBSERVED alias of a macro-bearing module -- ascribed inline or
   through the mli -- delivers the macros under the observing unit's
   name, so that unit's macros object carries a coerced reference to
   the target's, exactly as its run-time object does in the run world.
   An UNOBSERVED alias stays free: no object, uses normalise through.
   Both routes used to compile cleanly and strand every consumer at
   "Cannot find ...$macros.cmo". *)

let () =
  Printf.printf "inline: %d %d\n"
    $(Macro_alias_ascribed_inline.R.m ())
    $(Macro_alias_ascribed_inline.R.k << 7 >>);
  Printf.printf "mli: %d\n" $(Macro_alias_ascribed_mli.R.m ());
  Printf.printf "bare: %d\n" $(Macro_alias_ascribed_bare.R.m ())
