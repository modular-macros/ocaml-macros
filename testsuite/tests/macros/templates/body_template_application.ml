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

(* A template whose whole body is the application of a template (T2),
   also under a signature (T3).  The body returns the application's
   module; each application of the template expands it, bound to a name
   or included.  T4's applications expand U4 once each: the stamps
   differ, and each reads its own argument. *)

module type S = sig val x : int end
module U $(X : S) = struct let z = X.x * 10 end
module T2 $(X : S) = U$(X)
module T3 $(X : S) : sig val z : int end = U$(X)

module B = T2$(struct let x = 2 end)
module C = T3$(struct let x = 3 end)
include T2$(struct let x = 4 end)

let () = Printf.printf "%d %d %d\n" B.z C.z z

$let counter = ref 0
module type V = sig $val v : int end
module U4 $(X : V) = struct
  $let stamp = incr counter; !counter
  let s = $(Expr.int (X.v * 100 + stamp))
end
module T4 $(X : V) = U4$(X)
module D = T4$(struct $let v = 1 end)
module E = T4$(struct $let v = 2 end)

let () = Printf.printf "%d %d\n" D.s E.s
