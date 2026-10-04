(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A functor with compile-time components as a member of a recursive
   module, and as a recursive module. *)

module rec R : sig
  module F : functor (X : sig val v : int end) -> sig
    $val[..] g : unit -> int expr
    $val s : int
    val w : int -> int
  end
end = struct
  module F (X : sig val v : int end) = struct
    $let[..] g () = << X.v + S.t 1 >>
    $let s = 100
    let w n = X.v + S.t n
  end
end
and S : sig val t : int -> int  val u : int -> int end = struct
  module A = R.F (struct let v = 1000 end)
  let t n = n * 2
  let u n = A.w n + $(A.g ()) + $(Expr.int A.s)
end

module B = R.F (struct let v = 10 end)
let a = $(B.g ()) + $(Expr.int B.s) + B.w 3

module type V = sig val v : int end
module rec F : functor (X : V) -> sig
  $val[..] m : unit -> int expr
  val n : int
end = functor (X : V) -> struct
  $let[..] m () = << X.v + G.k () >>
  let n = X.v
end
and G : sig val k : unit -> int end = struct
  let k () = 5
end

module M = F (struct let v = 3 end)
let b = $(M.m ()) + M.n

let () = Printf.printf "%d %d %d\n" a (S.u 4) b
