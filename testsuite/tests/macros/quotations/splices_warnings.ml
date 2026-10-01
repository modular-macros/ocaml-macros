(* TEST
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 {
   flags = "-w -51";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   flags = "-warn-error +51";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/splices_warnings.error.reference";
   check-ocamlc.byte-output;
 }
*)

(* The warning settings of the command line govern the warnings of a
   unit with a top-level splice, as of any unit: here warning 51, which
   the simplifier emits. *)

let rec f x = if x = 0 then 0 else ((f [@tailcall]) (x - 1)) + 1
let k = $(<< 3 >>)
