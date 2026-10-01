(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* Expr.float lifts negative zero exactly. *)

let () = Printf.printf "%g %g\n" (1. /. $(Expr.float (-0.))) (1. /. $(Expr.float 0.))
