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

(* Static bindings ($let): ordinary values of the COMPILE world
   (STATIC-BINDINGS.md).  No closure conversion, no calling
   convention, general patterns and recursion allowed -- everything a
   [let] can do, one level down.  The values live in the macros
   object; the run-time slots are dummies nothing reads.  Quotations
   in an initialiser are allowed when they capture only global names
   (D2): [one] quotes a literal and is spliced through a $gen.  The
   $gen [m] reads the statics at level -1 like any compile-time
   value; both back ends run the same program, the compile world
   being bytecode everywhere. *)

$let three = 3
$let double x = x + x
$let (a, b) = (10, 20)
$let rec fact n = if n <= 1 then 1 else n * fact (n - 1)
$let one = << 1 >>

(* Nested in a module: the two-block model carries statics like any
   compile-time member (batch only; the toplevel refuses this form). *)
module M = struct $let z = 6 let r = 9 end

$gen m () : int expr = Expr.int (double three + a + b + fact 4)
$gen q () : int expr = one
$gen n () : int expr = Expr.int (M.z * 7)

let () = Printf.printf "%d %d %d %d\n" $(m ()) $(q ()) $(n ()) M.r
