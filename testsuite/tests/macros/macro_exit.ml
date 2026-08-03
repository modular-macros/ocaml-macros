(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The object of a module with a top-level splice is written BY its static
   program, from that program's last continuation.  A macro that calls
   [exit] therefore ends the process before the emit call, and a driver
   that reads the zero exit status as success reported a successful
   compilation with no .cmo written -- after which a build system either
   fails later with a confusing missing-file error or, worse, links a
   stale object from an earlier build.  [exit 0] is what a library's
   "print help and stop" path does.

   The artifact, not the exit status, is now the success criterion: the
   object is removed before the run, so its absence afterwards is proof
   the run did not write it.  [exit 1] and any other nonzero status were
   always diagnosed, through the status. *)

macro m () = (exit 0 : int expr)
let x = $(m ())
let () = print_int x
