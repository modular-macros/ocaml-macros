(* TEST
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Objects are not supported in generators. *)

$let[..] g () = let o = object method m = 1 end in Expr.int o#m
