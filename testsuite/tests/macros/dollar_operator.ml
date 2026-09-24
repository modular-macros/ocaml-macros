(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Bare $ is an infix operator again; splicehood is glued spelling
   (the Template Haskell convention, after Olivier Nicole's patch on
   the ocaml-macros prototype): $x, $M.x and $( e ) splice, while a
   free-standing $ is whatever the scope defines, and the forms
   coexist in one expression.  The $-keywords ($let, $val) are
   single tokens and unaffected. *)

let ( $ ) f x = f x

$let s = 5
module K = struct $let e = Expr.int 7 end

$gen m () : int expr = Expr.int s
$gen tw x = << $x + $x >>
$gen g () : int expr = << $K.e * 2 >>

let a = succ $ 41
let b = succ $ $(m ())
let c = $(tw (Expr.int 2))

let () = Printf.printf "%d %d %d %d\n" a b c $(g ())
