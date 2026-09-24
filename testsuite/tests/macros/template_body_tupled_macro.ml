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

(* A tuple-pattern $gen in a TEMPLATE FUNCTOR body, under ocamlopt.
   The template body's record pass runs inside whichever driver
   translates the enclosing unit, and under the native driver its
   ambient native_code gave the $gen's compile-time function the
   tupled convention -- the calling-convention family of
   macro_tupled_param.ml, reached through the one compile-world
   translation that lacked the bytecode forcing.  The defensive check
   added with that fix turned what would have been a static-program
   segfault into "Translmod: a $gen's compile-time function is
   tupled", which is how the flap and frex ports found it.  The pass
   is now translated in bytecode mode like its three siblings; the
   splice exercises a tuple-pattern closure on the same path, and the
   calls check values on both back ends. *)

module type S = sig $gen v : unit -> int expr end

module F [X : S] = struct
  $gen k : int * int -> int = fun (a, b) -> a + b
  $gen k3 : int * int * int -> int = fun (a, b, c) -> a * b + c
  let r = $( let f = fun (a, b) -> << $a + $b >> in f (X.v (), X.v ()) )
end

module V = struct $gen v () = << 5 >> end
module M = F[V]

let n = $( Expr.int (M.k (3, 4)) )
let n3 = $( Expr.int (M.k3 (2, 3, 1)) )
let () = Printf.printf "%d %d %d\n" M.r n n3
