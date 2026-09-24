(* TEST
 readonly_files = "static_let_import_lib.ml static_let_import_lib.mli";
 setup-ocamlc.byte-build-env;
 all_modules = "static_let_import_lib.mli static_let_import_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 {
   all_modules = "static_let_import.ml";
   compile_only = "true";
   ocamlc.byte;
   {
     all_modules = "static_let_import_lib.cmo static_let_import.cmo";
     compile_only = "false";
     flags = "";
     ocamlc.byte;
     run;
     check-program-output;
   }
 }
*)

(* Statics across units: the values live in the library's macros
   object, reached by ordinary global projection at level -1 -- the
   $macros twin is pulled in through the relocation walk like a
   $gen's, with no flags needed.  The library's run-time member is
   still a run-time member (dual worlds in one interface, one cmi). *)

$gen m () : int expr =
  Expr.int (Static_let_import_lib.next Static_let_import_lib.v)

let () =
  Printf.printf "%d %d\n" $(m ()) Static_let_import_lib.runtime
