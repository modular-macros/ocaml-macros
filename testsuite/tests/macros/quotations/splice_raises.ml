(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An exception that compile-time code lets escape is reported at the
   splice. *)

let a = $(if List.length [] = 0 then failwith "boom" else << 1 >>)
