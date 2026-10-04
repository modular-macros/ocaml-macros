(* TEST
 readonly_files = "load_lib.ml";
 setup-ocamlc.byte-build-env;
 module = "load_lib.ml";
 compile_only = "true";
 ocamlc.byte;
 flags = "-I .";
 ocaml;
 check-ocaml-output;
*)

(* Loading a unit loads its compile-time part too. *)

#load "load_lib.cmo";;
let a = $(Load_lib.add << 1 >>) + $(Expr.int Load_lib.shift);;
$let[..] twice x = Load_lib.add (Load_lib.add x);;
let b = $(twice << 0 >>);;
include Load_lib;;
let c = $(add << base >>);;
