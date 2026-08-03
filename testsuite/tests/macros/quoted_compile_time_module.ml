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

module Mv = struct let v = 5 end

macro m () = let module N = Mv in << N.v >>
