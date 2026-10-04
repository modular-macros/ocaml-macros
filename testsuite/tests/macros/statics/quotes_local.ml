(* TEST
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A quotation in a static binding may mention the run-time values of
   other units, but not those of its own unit. *)

let local = 7
$let good = << Stdlib.succ 1 >>
$let bad = << local + 1 >>
