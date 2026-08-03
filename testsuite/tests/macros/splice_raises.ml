(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An exception a macro lets escape fails the compilation with an error
   located at the splice, reported through the static program's error
   file (bytecomp/static_report.mli) -- not as an uncaught exception in a
   program the user never wrote.  The static program's debris is cleaned
   up on this path. *)

macro boom () = (failwith "boom" : int expr)

let ok = $( << 1 >> )

let x = $( boom () )
