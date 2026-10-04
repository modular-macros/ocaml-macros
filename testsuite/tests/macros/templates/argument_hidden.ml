(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* The argument of a template may be an application whose own argument
   has compile-time bindings that its signature hides. *)

module Id (X : sig val w : int end) = X
module T $(X : sig val w : int end) = struct let z = X.w end
module M = T$(Id (struct $let s = 10  let w = $(Expr.int s) end))
let () = print_int M.z
