(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A signature may hide the compile-time bindings of a module in the
   body of a functor: the splices of the module use them all the
   same. *)

module F (X : sig val v : int end) = struct
  module In : sig val w : int end = struct
    $let[..] g () = << X.v + 1 >>
    let w = $(g ())
  end
end
module A = F (struct let v = 1 end)

module G (X : sig val v : int end) = struct
  module In = struct
    module Deep : sig val w : int end = struct
      $let s = 10
      let w = $(Expr.int s) + X.v
    end
  end
  let u = In.Deep.w
end
module B = G (struct let v = 1 end)

(* The argument of a functor may be such a module. *)

module Id (X : sig val w : int end) = X
module C = Id (struct $let s = 40  let w = $(Expr.int s) end)
module H (Y : sig val y : int end) = struct
  module N = Id (struct $let[..] g () = << Y.y >> let w = $(g ()) + 1 end)
end
module D = H (struct let y = 50 end)

(* So may the body of a functor that is an application, a functor that
   is applied where it is written, and the definition of a recursive
   module. *)

module K (Y : sig val y : int end) =
  Id (struct $let s = 60  let w = $(Expr.int s) + Y.y end)
module E = K (struct let y = 1 end)

module L =
  (functor (X : sig end) ->
     (struct $let s = 70  let w = $(Expr.int s) end : sig val w : int end))
    (struct end)

module rec R : sig val w : int end =
  Id (struct $let s = 80  let w = $(Expr.int s) end)
and S : sig val k : unit -> int end = struct let k () = R.w end

(* An application may be included, and opened. *)

include Id (struct $let s = 90  let w = $(Expr.int s) end)
let included = w
open Id (struct $let s = 100  let w = $(Expr.int s) end)

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d\n"
    A.In.w B.u C.w D.N.w E.w L.w (S.k ()) included w
