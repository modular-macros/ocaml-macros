(* TEST
 {
   setup-ocamlc.byte-build-env;
   flags = "-nopervasives";
   compile_only = "true";
   ocamlc.byte;
   flags = "";
   compile_only = "false";
   all_modules = "compile_time_stdlib_nopervasives.cmo";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   flags = "-nopervasives";
   compile_only = "true";
   ocamlopt.byte;
   flags = "";
   compile_only = "false";
   all_modules = "compile_time_stdlib_nopervasives.cmx";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Without the initial open of the standard library, its units still
   serve compile-time code through qualified paths, as they serve
   run-time code.  (The unit is linked without the flag, which would
   leave the standard library out.) *)

let a = $(Stdlib.Expr.int (Stdlib.List.length [1; 2]))
let () = Stdlib.print_int a; Stdlib.print_newline ()
