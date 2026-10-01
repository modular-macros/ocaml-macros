(* TEST
 flags += " -g ";
 debugger_script = "${test_source_directory}/code_values.input";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)

(* The debugger prints a code value as [<code>]: the value lives in the
   debugged program, where the toplevel's printing of its term is not
   available. *)

let f (c : (int -> int) expr) (l : int expr list) =
  print_endline "f";
  ignore (c, l)

let () = f << fun x -> x + 1 >> [ << 1 >>; << 2 >> ]
