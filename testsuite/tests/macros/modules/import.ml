(* TEST
 readonly_files = "import_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "import_lib.ml import.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "import_lib.ml import.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The modules of another unit in compile-time code: a module of static
   bindings, packed for the compile-time code of the other unit, in a
   generator and at a splice; its packages unpacked; its modules
   aliased. *)

module L = Import_lib

module Mine = struct $let v = 41 end
$let[..] g () = Expr.int (L.run (module Mine : L.S) + L.run (module L.Static : L.S))
let a = $(g ())
let b = $(Expr.int (L.run (module Mine : L.S)))
$let c = let module M = (val L.make 7) in M.v
$let d = let module I = L.Static in I.v

let () = Printf.printf "%d %d %d %d\n" a b $(Expr.int c) $(Expr.int d)
