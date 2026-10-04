(* TEST
 readonly_files = "import_base.ml import_lib.mli import_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "import_base.ml import_lib.mli import_lib.ml import.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "import_base.ml import_lib.mli import_lib.ml import.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
*)

(* The splices of a unit may use the static bindings of other units.
   The compile-time parts of those units are loaded when the unit is
   compiled, together with the compile-time parts that they use in
   turn; their run-time parts are not. *)

module L = Import_lib
module A = L.Sub

let a = $(Expr.int Import_lib.x)
let b = $(Expr.int (L.Sub.y + A.y))
open Import_lib
let c = $(Expr.int (x + Sub.y))

let () =
  Printf.printf "%d %d %d %d %d %d\n" a b c L.v L.Sub.u L.Plain.t
