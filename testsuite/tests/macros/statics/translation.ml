(* TEST
 readonly_files = "translation_lib.mli translation_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "translation_lib.mli translation_lib.ml translation.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "translation_lib.mli translation_lib.ml translation.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The run-time part of a unit with static bindings: the static
   bindings keep their positions in the block of the unit, and are not
   evaluated at run time. *)

module L = Translation_lib

let () =
  Printf.printf "%d %d %d %d %d %d %d\n"
    L.a L.b L.Sub.d L.Statics.e L.Restricted.v L.h (L.f 0);
  Printf.printf "%d\n" L.Ints.(cardinal (singleton 1));
  try raise L.E with L.E -> print_endline "E"
