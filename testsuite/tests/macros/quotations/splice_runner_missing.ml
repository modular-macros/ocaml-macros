(* TEST
 setup-ocamlc.byte-build-env;
 {
   (* A copy of the compiler in a directory with no compiler libraries
      beside it is taken for an installed compiler, and its runner is
      looked for beside it. *)
   script = "cp ${ocamlc_byte} ocamlc-copy";
   script;
 }{
   output = "${test_build_directory}/splice_runner_missing.output";
   exit_status = "2";
   script = "${ocamlrun} ./ocamlc-copy -nostdlib -I ${ocamlsrcdir}/stdlib \
             -c splice_runner_missing.ml";
   script;
   check-program-output;
 }
*)

(* The static program is run by a runner built with the compiler and
   found beside it; a compiler with no runner beside it reports the
   runner's absence before running anything. *)
let x = $( << 1 >> )
let () = print_int x
