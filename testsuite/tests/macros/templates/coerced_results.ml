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

(* Templates coerced where they are not defined, with results that the
   coercion used to mishandle.

   A result that is a functor was not coerced at all in the program
   generated: a narrowing that moved a field gave the value of another,
   silently, and one that made a field of an alias member crashed.

   A member with no compile-time content is a dummy in the
   application's record, which the coercion of the record took apart:
   the static program was killed by a segmentation fault. *)

module type S = sig
  type t
  val v : int
  $val k : int
  module Sub : sig val z : int  val y : int end
end
module type T = sig val n : int  val m : int end

module K = struct
  $let other : int = 8
  $let k : int = 7
  let w = 100
  let v = 5
  $let[..] g c = << $c + v + w >>
end

(* A functor as the result: a moved field, an alias member, generators. *)
module type HF = sig
  module F : $(X : S) -> (Y : T) -> sig
    val b : int
    $val[..] plus : int expr -> int expr
    module M : sig val v : int  $val[..] g : int expr -> int expr end
  end
end
module IdF $(X : S) (Y : T) = struct
  let a = 1
  let b = 2 + Y.n
  $let[..] plus c = << $c + b + a >>
  module M = K
end
module HF1 : HF = struct module F = IdF end

(* A functor as a member of the result. *)
module type HG = sig
  module F : $(X : S) -> sig
    module G : (Y : T) -> sig
      module N : sig $val k : int  val v : int end
      val sum : int
    end
  end
end
module IdG $(X : S) = struct
  module G (Y : T) = struct
    let first = 0
    module N = K
    let sum = first + Y.n + K.w
  end
end
module HG1 : HG = struct module F = IdG end

(* Members copied from the parameters: a copy with a static, and copies
   with no compile-time content. *)
module type HP = sig
  module F : $(X : S) -> (Y : T) -> sig
    module MX : sig val v : int  $val k : int end
    module MS : sig val y : int end
    module MY : sig val m : int end
  end
end
module IdP $(X : S) (Y : T) = struct
  module MX = X
  module MS = X.Sub
  module MY = Y
end
module HP1 : HP = struct module F = IdP end

module type HQ = sig
  module F : $(X : S) -> sig module MS : sig val y : int end end
end
module IdQ $(X : S) = struct module MS = X.Sub end
module HQ1 : HQ = struct module F = IdQ end

module A = struct
  type t = int
  let extra = 0
  let v = 5
  $let k : int = 7
  module Sub = struct let z = 1 let y = 2 end
end
module W = struct let n = 40 let m = 41 end

module FX = HF1.F$(A)
module F = FX(W)
module G0 = HG1.F$(A)
module G = G0.G(W)
module PX = HP1.F$(A)
module P = PX(W)
module Q = HQ1.F$(A)

let () =
  Printf.printf "%d %d %d %d\n" F.b $(F.plus << 1 >>) F.M.v $(F.M.g << 1 >>);
  Printf.printf "%d %d %d\n" $(Expr.int G.N.k) G.N.v G.sum;
  Printf.printf "%d %d %d %d %d\n"
    P.MX.v $(Expr.int P.MX.k) P.MS.y P.MY.m Q.MS.y
