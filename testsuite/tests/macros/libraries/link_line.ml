(* TEST
 subdirectories = "lib";
 readonly_files = "link_line_one.ml";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "lib/counter.ml lib/twice.ml";
 flags = "-I lib";
 ocamlc.byte;
 all_modules = "link_line_one.ml";
 flags = "-I-static lib";
 ocamlc.byte;
 all_modules = "link_line.ml";
 flags = "-I-static lib -static-use counter.cmo -static-use twice.cmo";
 ocamlc.byte;
 all_modules = "link_line_one.cmo link_line.cmo";
 compile_only = "false";
 flags = "";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* The files given to -static-use are those that the compile-time code
   of the unit needs, in link order: for its own uses, and for those of
   the compile-time code of the units that it depends on. *)

let a = $(Link_line_one.one ())
let b = $(Expr.int (Twice.next ()))
let () = Printf.printf "%d %d\n" a b
