(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A template application is translated as an item, and inside the
   module expression of an item; as the right-hand side of a recursive
   module binding it is refused. *)

module F $(X : sig end) = struct let v = 1 end
module rec A : sig val v : int end = F$(struct end)
and B : sig end = struct end
