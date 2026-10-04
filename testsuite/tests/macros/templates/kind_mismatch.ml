(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A template is applied with a dollar. *)

module type S = sig type t end
module T $(X : S) = struct type t = X.t end
module Bad = T(struct type t = int end)
