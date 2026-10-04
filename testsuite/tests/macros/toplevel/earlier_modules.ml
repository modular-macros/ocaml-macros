(* TEST
 toplevel;
*)

(* A module of an earlier phrase that has no compile-time components
   may be a member of a module that has some, under a signature that
   narrows it. *)

module M = struct let v = 5  let w = 6  module Sub = struct let z = 7 end end;;
module N : sig module Q : sig val w : int end  $val[..] g : unit -> int expr end
  = struct module Q = M  $let[..] g () = << 1 >> end;;
N.Q.w + $(N.g ());;
module M2 = struct module Q = M  $let j : int = 1 end;;
module N2 : sig module Q : sig val w : int end  $val j : int end = M2;;
N2.Q.w + $(Expr.int N2.j);;
module N3 : sig module Q : sig val z : int end  $val j : int end = struct module Q = M.Sub  $let j = 2 end;;
N3.Q.z + $(Expr.int N3.j);;
module L = struct module Q = M end;;
module N4 : sig module L : sig module Q : sig val w : int end end $val j : int end = struct module L = L $let j = 3 end;;
N4.L.Q.w + $(Expr.int N4.j);;
include (struct module Q = M $let k = 4 end : sig module Q : sig val w : int end $val k : int end);;
Q.w + $(Expr.int k);;
