(* TEST
 flags += " -g ";
 debugger_script = "${test_source_directory}/spliced_unit.input";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)

(* A unit with a top-level splice keeps its debug events: the debugger
   stops in it and shows its variables. *)

let f x =
  let y = x + $(Expr.int 40) in
  print_int y; print_newline ()

let () = f 2
