(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Objects are not supported in compile-time code. *)

let a = $(let o = object method m = << 1 >> end in o#m)
