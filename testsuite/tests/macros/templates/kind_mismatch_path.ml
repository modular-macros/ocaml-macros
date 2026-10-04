(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The path of a template application is written with a dollar. *)

module type S = sig type t end
module T $(X : S) = struct type t = X.t end
module M = struct type t = int end
type bad = T(M).t
