(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A signature may hide the static bindings of a module: the splices of
   the module use them all the same. *)

module M : sig val w : int end = struct
  module N = struct $let s = 1 end
  let w = $(Expr.int N.s)
end

open (struct $let s = 20  let w = $(Expr.int s) end : sig val w : int end)

include (struct $let s = 30  let x = $(Expr.int s) end : sig val x : int end)

let () = Printf.printf "%d %d %d\n" M.w w x
