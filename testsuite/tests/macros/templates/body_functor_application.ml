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

(* A template whose whole body is the application of a functor without
   compile-time components.  The body binds nothing at compile time, so
   its record is a placeholder, and the functor's placeholder is not
   applied.  Each application of the template is bound to a name or
   included. *)

module type S = sig val x : int end
module F (X : S) = struct let y = X.x + 1 end
module T $(X : S) = F (X)
module A = T$(struct let x = 1 end)
include T$(struct let x = 5 end)

let () = Printf.printf "%d %d\n" A.y y
