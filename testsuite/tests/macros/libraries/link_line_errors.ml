(* TEST
 subdirectories = "lib";
 readonly_files = "link_line_one.ml";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "lib/counter.ml lib/twice.ml";
 flags = "-I lib";
 ocamlc.byte;
 all_modules = "link_line_one.ml";
 flags = "-I-static lib -static-use counter.cmo";
 ocamlc.byte;
 all_modules = "link_line_errors.ml";
 ocamlc_byte_exit_status = "2";
 {
   flags = "-I-static lib -static-use twice.cmo -static-use counter.cmo";
   ocamlc.byte;
 }{
   flags = "-I-static lib -static-use twice.cmo";
   ocamlc.byte;
 }{
   flags = "-I-static lib";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* The files given to -static-use are linked in the order given.  A
   file that uses a unit of a later file is refused.  Nothing is
   remembered from the compilation of another unit: the files that its
   compile-time code needs must be given again. *)

let a = $(Link_line_one.one ())
let b = $(Expr.int (Twice.next ()))
