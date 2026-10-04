(* TEST
 flags = "-c -dlambda -dno-unique-ids -dno-locations";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/splices_dlambda.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   no-flambda;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   compiler_reference = "${test_source_directory}/splices_dlambda.ocamlopt.reference";
   check-ocamlopt.byte-output;
 }
*)

(* The term that -dlambda prints for a unit with a top-level splice is
   that of the unit, the splice filled with its code. *)

let x = 2
let f y = y + $(Expr.int 40)
let () = print_int (f x)
