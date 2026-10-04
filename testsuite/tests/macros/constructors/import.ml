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

(* The compile-time constructors of another unit are constructors of
   the compile-time code of this unit, qualified, opened, included or
   rebound. *)

module L = Import_lib

$let a = try L.stop 1 with L.Stop n -> n
let b = $(L.guarded << L.fail 2 >>)

open Import_lib
$let c = try stop 3 with Stop n -> n
$let d = match A with A -> 4 | _ -> 0
let e = match B with B -> 5 | _ -> 0

exception $Halt = Stop
exception Broken = Failed
$let f = try stop 6 with Halt n -> n
let g = try fail 7 with Broken n -> n

type t += $C
$let h = match C with A -> 0 | C -> 8 | _ -> 0

module I = struct include Import_lib end
$let i = try stop 9 with I.Stop n -> n

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d\n"
    $(Expr.int a) b $(Expr.int c) $(Expr.int d) e $(Expr.int f) g
    $(Expr.int h) $(Expr.int i)
