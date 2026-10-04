(* TEST
 flags = "-stop-after parsing";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The mark of a generator belongs to the keyword, not to "rec". *)

$let rec [..] a x = << $x >>
