(* TEST
 setup-ocamlc.byte-build-env;
 commandline = "-depend -modules depend.ml";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/depend.reference";
 check-ocamlc.byte-output;
*)

(* ocamldep finds the modules that quotations and splices name, in the
   splice shorthand too. *)

let a = << Quoted.x >>
let b = $Shorthand.y
let c = $(Spliced.f << Inner.z >>)
