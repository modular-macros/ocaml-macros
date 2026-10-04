(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A template cannot be applied in a module bound in an expression. *)

module type S = sig val v : int end
module G $(X : S) = struct let g = X.v * 2 end
module V = struct let v = 1 end
let z = let module I = struct module A = G$(V) end in I.A.g
