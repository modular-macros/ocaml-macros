(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 all_modules = "lib/counter.ml lib/both.ml";
 ocamlc.byte;
 all_modules = "typing.ml";
 flags = "-I lib -I-static lib -i";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The run-time components of a unit that is available to compile-time
   code are typed there as compile-time components. *)

$let a = Counter.next ()
$let[..] b (v : Counter.t) = Expr.string (Counter.name v)
$let c = Both.v + Both.s
open Both
$let d = v + s + Sub.w + Sub.t
let e = v + Sub.w
