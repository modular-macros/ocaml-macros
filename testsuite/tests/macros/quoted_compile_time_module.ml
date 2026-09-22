(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A module BOUND in compile-time code cannot be named by the code a
   quotation builds: that world has no incarnation of it.  Before the check
   the reference reached the translation, where the projection's root had an
   address in neither world, and the static program segfaulted. *)

(* The binding itself must be admissible in compile-time code (a
   run-world module is refused at the binding since 2026-09-15), so
   it aliases a module served there; the quotation guard is what this
   test pins. *)
$gen m () = let module N = Stdlib.Int in << N.zero >>
