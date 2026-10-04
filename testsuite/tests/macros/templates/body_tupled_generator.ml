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

(* A tuple-pattern generator in a template body, under ocamlopt: the
   body's compile-time pass is translated in bytecode mode whichever
   compiler translates the unit, so the generator's function keeps
   the curried convention.  The splice exercises a tuple-pattern
   closure on the same path, and the calls check values on both back
   ends. *)

module type S = sig $val[..] v : unit -> int expr end

module F $(X : S) = struct
  $let[..] k : int * int -> int = fun (a, b) -> a + b
  $let[..] k3 : int * int * int -> int = fun (a, b, c) -> a * b + c
  let r = $( let f = fun (a, b) -> << $a + $b >> in f (X.v (), X.v ()) )
end

module V = struct $let[..] v () = << 5 >> end
module M = F$(V)

let n = $( Expr.int (M.k (3, 4)) )
let n3 = $( Expr.int (M.k3 (2, 3, 1)) )
let () = Printf.printf "%d %d %d\n" M.r n n3
