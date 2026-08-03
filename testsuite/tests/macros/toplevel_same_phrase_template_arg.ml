(* TEST
 toplevel;
*)

(* A template functor argument with both run-time and macro
   components, defined by the same phrase as the application: since
   the stage -1 layout-parity fix the static block shares the full
   layout, so the compile-time part's adapted view of the argument is
   position-correct and the former "must be defined by an earlier
   phrase" guard is gone.  The argument's macro may even quote the
   argument's own run-time components. *)

module F[X : sig val r : int macro g : unit -> int expr end] =
  struct let s = $(X.g ()) + X.r end;;

(* Definition and application in one phrase; the macro quotes nothing. *)
module V = struct let r = 1 macro g () = << 2 >> end
module M1 = F[V];;
Printf.printf "a=%d\n" M1.s;;

(* The argument's macro quotes the argument's own run-time value. *)
module W = struct let r = 40 macro g () = << r >> end
module M2 = F[W];;
Printf.printf "b=%d\n" M2.s;;

(* Definition, application and use all in one phrase. *)
module U = struct let r = 5 macro g () = << r * 2 >> end
module M3 = F[U]
let z = M3.s + U.r;;
Printf.printf "c=%d\n" z;;
