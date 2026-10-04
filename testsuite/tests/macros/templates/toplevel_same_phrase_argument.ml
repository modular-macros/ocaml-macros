(* TEST
 toplevel;
*)

(* A template argument with both run-time and generator components,
   defined by the same phrase as the application: the compile-time
   block of a phrase shares the layout of its run-time block, so the
   compile-time view of the argument is position-correct.  The
   argument's generator may quote the argument's own run-time
   components. *)

module F$(X : sig val r : int $val[..] g : unit -> int expr end) =
  struct let s = $(X.g ()) + X.r end;;

(* Definition and application in one phrase; the $let[..] quotes nothing. *)
module V = struct let r = 1 $let[..] g () = << 2 >> end
module M1 = F$(V);;
Printf.printf "a=%d\n" M1.s;;

(* The argument's $let[..] quotes the argument's own run-time value. *)
module W = struct let r = 40 $let[..] g () = << r >> end
module M2 = F$(W);;
Printf.printf "b=%d\n" M2.s;;

(* Definition, application and use all in one phrase. *)
module U = struct let r = 5 $let[..] g () = << r * 2 >> end
module M3 = F$(U)
let z = M3.s + U.r;;
Printf.printf "c=%d\n" z;;
