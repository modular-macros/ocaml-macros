(* TEST
 readonly_files = "import_lib.mli import_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "import_lib.mli import_lib.ml import.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "import_lib.mli import_lib.ml import.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The generators of another unit are called from splices, and from
   the generators of this unit.  The code that they build reaches the
   values of their unit, including those that its interface hides. *)

module L = Import_lib

let a = $(Import_lib.add << 1 >>)
let b = $(L.add_twice << 1 >>)
let c = $(L.rec_add 3 << 0 >>)
let d = $(L.Sub.mul << 2 >>)

let local = 5
$let[..] both x = L.Sub.mul (L.add << $x + local >>)
$let[..] shifted x = << $x + $(Expr.int L.shift) >>

let e = $(both << 0 >>)
let f = $(shifted << 1 >>)

open Import_lib
let g = $(add << visible >>)

let () = Printf.printf "%d %d %d %d %d %d %d\n" a b c d e f g
