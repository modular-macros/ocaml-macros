(* TEST
 setup-ocamlc.byte-build-env;
 {
   (* A copy of the compiler in a directory with no compilerlibs beside
      it: the layout probe then reads "installed", and every companion --
      the runner among them -- is looked for beside the executable. *)
   script = "cp ${ocamlc_byte} ocamlc-copy";
   script;
 }{
   output = "${test_build_directory}/static_runner_missing.output";
   exit_status = "2";
   script = "${ocamlrun} ./ocamlc-copy -nostdlib -I ${ocamlsrcdir}/stdlib \
             -c static_runner_missing.ml";
   script;
   check-program-output;
 }
*)

(* The static program is not run by this process: it is handed to
   staticrun, a small prebuilt driver that lives beside the compiler and
   is found from the same root the compile-world archives are found from
   (a build tree, or an installation).  A compiler moved out of that root
   -- copied somewhere on its own, or an installation whose runner did
   not get installed -- has no runner to hand it to.  Spawned anyway, the
   absence would arrive as the runtime's own message on the child's
   stderr and a status this process could only report as a failed
   compile-time evaluation, naming the user's own macro.  It is checked
   before the spawn instead, and the file is named. *)

let x = $( << 1 >> )
let () = print_int x
