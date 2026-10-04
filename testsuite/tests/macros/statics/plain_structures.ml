(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A structure that binds nothing at compile time is a placeholder in
   the compile-time part, whatever item binds it: its splices run all
   the same. *)

$let a = 1
module M = struct
  let x = $(Expr.int a)
  let y = x + 1
end
include struct let z = $(Expr.int (a + 10)) end
open struct let w = $(Expr.int (a + 20)) end
module rec R : sig val r : int end = struct let r = $(Expr.int (a + 30)) end
module N = struct module P = struct let p = $(Expr.int (a + 40)) end end
let () = Printf.printf "%d %d %d %d %d %d\n" M.x M.y z w R.r N.P.p
