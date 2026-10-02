(* TEST
 subdirectories = "lib";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/counter.ml lib/both.ml";
 compile_only = "true";
 ocamlc.byte;
 all_modules = "alias.ml";
 flags = "-I lib -I-static lib -static-use counter.cmo -static-use both.cmo";
 ocamlc.byte;
 all_modules = "lib/counter.cmo lib/both.cmo alias.cmo";
 compile_only = "false";
 flags = "";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* An alias of a unit that is available to compile-time code and to
   run-time code, or of one of its modules, serves both as the unit
   does. *)

module A = Both
module B = A
module N = struct module S = B.Sub end

$let a = A.v + B.s + N.S.w + N.S.t
$let[..] b () = B.add (Expr.int A.Sub.w)

open A
$let c = v + Sub.w

let () =
  Printf.printf "%d %d %d %d\n"
    $(Expr.int a) $(b ()) $(Expr.int c) (A.v + N.S.w)
