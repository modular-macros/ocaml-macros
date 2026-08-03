(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A splice in a macro body, outside any quotation, has no meaning:
   there is no enclosing quotation to splice into and no run-time term
   to hold a hole.  It used to be given a top-level splice index that
   translation could never fill, dying with an uncaught
   "splice source is not initialized". *)

macro m () = $( << 1 >> )
let () = print_int 0
