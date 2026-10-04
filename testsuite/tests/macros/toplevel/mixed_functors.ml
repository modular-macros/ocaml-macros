(* TEST
 toplevel;
*)

(* A functor whose body has compile-time components, in each position
   in which a phrase may define, apply or rebind one. *)

module F (X : sig val v : int end) = struct
  $let[..] add x = << $x + X.v >>
  $let scale = 10
  let run = 100 + X.v
end;;
module A = F (struct let v = 1 end);;
let a = $(A.add << A.run >>) + $(Expr.int A.scale);;

(* In a structure, named or not *)

module Outer = struct
  module G (X : sig val v : int end) = struct
    $let[..] mul x = << $x * X.v >>
  end
  module B = G (struct let v = 3 end)
  let b = $(B.mul << 2 >>)
end;;
module C = Outer.G (struct let v = 5 end);;
let c = $(C.mul << Outer.b >>);;

module _ = struct
  module H (X : sig val v : int end) = struct
    $let[..] sub x = << $x - X.v >>
  end
  module D = H (struct let v = 1 end)
  let () = Printf.printf "d = %d\n" $(D.sub << 10 >>)
end;;

(* An application that is opened, or included *)

open F (struct let v = 20 end);;
let e = $(add << run >>);;
include F (struct let v = 30 end);;
let f = $(add << run >>) + $(Expr.int scale);;

(* An include of a module that has such a functor as a member *)

include Outer;;
module E = G (struct let v = 7 end);;
let g = $(E.mul << b >>);;
