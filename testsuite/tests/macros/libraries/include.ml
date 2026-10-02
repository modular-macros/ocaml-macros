(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml lib/both.ml";
 compile_only = "true";
 ocamlc.byte;
 all_modules = "include.ml";
 flags = "-I lib -I-static lib -static-use counter.cmo -static-use both.cmo";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An include behaves as the rebinding of each component: of a unit
   that is available to compile-time code and to run-time code, it
   binds each value as a run-time value of the including structure,
   which compile-time code cannot use. *)

module L = struct include Both end
let a = L.v
$let b = L.s
$let c = L.v
