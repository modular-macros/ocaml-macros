(* TEST
 flags = "-static-use nosuch.cma";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* -static-use names the compile-world archives a unit's static program
   links.  An implicit name is resolved through the load path (which
   -I-static extends), so a misspelling, or a directory the build forgot
   to add, gives an archive that is simply not there.  Resolving it late,
   inside the child link, would have surfaced as the runner's own
   "cannot find" text with no location; it is resolved here, in the
   parent, so the failure is an ordinary located compiler error naming
   the argument as the user wrote it. *)

let x = $( << 1 >> )
let () = print_int x
