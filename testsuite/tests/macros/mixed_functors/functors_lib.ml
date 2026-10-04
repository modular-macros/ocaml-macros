(* Functors with compile-time components: with a hidden value and a
   splice in the body, curried, generative, and under a signature that
   hides and reorders. *)

let hidden = 1000
module F (X : sig val v : int end) = struct
  let local = X.v * 2
  $let[..] add x = << $x + X.v + local + hidden >>
  $let[..] twice x = add (add x)
  $let scale = print_endline "F.scale evaluated"; 10
  let w = $(add << 0 >>)
end
module Curried (X : sig val a : int end) (Y : sig val b : int end) = struct
  $let[..] sum () = << X.a + Y.b >>
end
module Gen () = struct
  let state = ref 0
  $let[..] bump () = << (incr state; !state) >>
end
module Sealed : functor (X : sig val v : int end) -> sig
  $val[..] second : unit -> int expr
  val first : int
end = functor (X : sig val v : int end) -> struct
  let first = X.v
  $let[..] helper () = << X.v + 1 >>
  $let[..] second () = << $(helper ()) + first >>
end
module Applied = F (struct let v = 5 end)
