(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The nonzero sibling of macro_exit.ml.  A macro that calls [exit] with a
   failing status -- a library's "report and die" path, reached at compile
   time -- ends the static program before it emits anything, and the status
   is all the parent has to go on: the child died out of reach of this
   process's reporting and left no located report behind.  The status is
   quoted as it stands, and the program's object and object list are KEPT,
   since with no diagnosis of its own they are the only evidence to debug
   with (a run that did diagnose itself cleans them up -- see
   splice_raises.ml). *)

macro bail () = (exit 3 : int expr)
let x = $(bail ())
let () = print_int x
