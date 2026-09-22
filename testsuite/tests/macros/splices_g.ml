(* TEST
   flags = "-g";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* As splices.ml, under -g: the run-time term handed to the static program
   must not carry debug events, whose environments cannot be serialised. *)

let x = $(<<3>>)
let () = Printf.printf "%d\n" x
