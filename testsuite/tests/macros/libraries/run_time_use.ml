(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "lib/counter.ml";
 ocamlc.byte;
 all_modules = "run_time_use.ml";
 flags = "-I-static lib";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The values of the units of a directory given to -I-static alone are
   not available to run-time code. *)

let n = Counter.next ()
