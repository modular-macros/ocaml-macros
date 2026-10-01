(* TEST
 flags = "-stop-after parsing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The closing bracket of a quotation cannot be defined as an
   operator either. *)

let ( >> ) f g x = g (f x)
