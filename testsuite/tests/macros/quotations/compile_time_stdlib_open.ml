(* TEST
 flags = "-open Stdlib.List";
 {
   bytecode;
 }{
   native;
 }
*)

(* A module of the standard library opened on the command line serves
   compile-time code as well as run-time code. *)

let a = length [1] + $(Expr.int (length [1; 2]))
let () = print_int a; print_newline ()
