(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml lib/both.ml";
 compile_only = "true";
 ocamlc.byte;
 ocaml;
 check-ocaml-output;
*)

(* The directive #static_dir makes the units of a directory available to
   compile-time code, and #static_use loads their implementations for
   it. *)

#static_dir "lib";;
$let a = Counter.next ();;
#static_use "absent.cmo";;
#static_use "counter.cmo";;
$let a = Counter.next ();;
let b = $(Expr.int (a + Counter.next ()));;
$let c = let open Counter in name B;;

(* Run-time code may use the types of the units, and no more. *)
Counter.next ();;
<< Counter.next () >>;;
module M = Counter;;
module type T = sig module M = Counter end;;
let f (x : Counter.t) = x;;
open Counter;;
$let d = name A;;
let g = [A; B];;
next ();;

(* Nor are their compile-time components available to compile-time
   code. *)
#static_use "both.cmo";;
$let h = Both.v;;
$let h = Both.s;;
$let h = let open Both in s;;

(* Loaded for run-time code too, they have another state there. *)
#directory "lib";;
#load "counter.cmo";;
let e = Counter.next () + $(Expr.int (Counter.next ()));;

(* The components of a unit that is available to both *)
#load "both.cmo";;
$let f = Both.v + Both.s + Both.Sub.w + Both.Sub.t;;
let g = $(Both.add (Expr.int Both.v)) + Both.v;;
module Included = struct include Both.Sub end;;
$let h = Included.t;;
let i = Included.w;;
