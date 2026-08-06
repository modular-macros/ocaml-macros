(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Template functor programs that must type check.  These cannot be
   written as expect tests yet: the expect harness runs the toplevel,
   which translates, and the translator does not support template
   functors.  Compiling with -stop-after typing exercises typing alone. *)

module type S = sig type t end
module type U = sig type u end
module type BIG = sig val v : int val w : int end
module type SMALL = sig val v : int end
module type WithMacro = sig macro m : int expr -> int expr end
module type WithVal = sig val v : int end

(* Definitions, in each of the parameter forms. *)
module F1[X : S] = struct type t = X.t end
module F2 = functor [X : S] -> struct type t = X.t end
module F3[] = struct type t = int end
module F4[_ : S] = struct type t = int end

(* A template functor may return a functor, and may mention macros. *)
module F5[X : S] = functor (Y : S) -> struct type t = X.t end
module F6[X : WithMacro] = struct let y = $(X.m << 3 >>) end
module F7[X : WithVal] = struct let y = X.v end

(* Applications. *)
module M = struct type u = int end
module G[X : U] = struct type t = X.u  type s end
module A = G[M]
module B = G[M]

(* A manifest equation mentioning the argument survives the
   application, in both of two separate applications. *)
let keep_equation_a (x : A.t) : int = x
let keep_equation_b (x : B.t) : int = x

(* Subtyping between template functors: the body is covariant ... *)
module Wide : [X : S] BIG = functor [X : S] -> struct let v = 1 let w = 2 end
module Narrow : [X : S] SMALL = Wide

(* ... and the parameter is contravariant. *)
module TakesSmall : [X : SMALL] S = functor [X : SMALL] -> struct type t end
module TakesBig : [X : BIG] S = TakesSmall

(* A positive but not strictly positive occurrence: under two
   parameters the variance flips back, so BIG is in a positive position
   again and the wider parameter may be supplied. *)
module Nested : [[X : BIG] S] S =
  functor [G : [X : BIG] S] -> struct type t end
module NestedOk : [[X : SMALL] S] S = Nested
