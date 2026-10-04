(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A template is a functor whose parameter may have compile-time
   components.  Inside the body the parameter's components serve at
   their levels; a template is a compile-time component itself. *)

module type S = sig type t end
module type V = sig val v : int end
module type G = sig $val[..] m : int expr -> int expr end
module type SV = sig $val v : int end

(* Definitions, in each form of parameter *)

module F1 $(X : S) = struct type t = X.t end
module F2 = functor $(X : S) -> struct type t = X.t end
module F3 $() = struct type t = int end
module F4 $(_ : S) = struct type t = int end
module F5 $(X : S) (Y : S) $(Z : S) $() = struct type t = X.t * Y.t * Z.t end
module F6 (X : V) $(Y : V) = struct let w = X.v + Y.v end

(* The parameter's components at their levels *)

module Uses $(X : G) = struct
  let a = $(X.m << 3 >>)
  $let[..] n x = X.m (X.m x)
  let b = $(n << 1 >>)
end
module Static $(X : SV) = struct
  $let s = X.v + 1
  let a = $(Expr.int s)
end
module Value $(X : V) = struct let w = X.v end

(* A template may take and return functors and templates of any
   order, and may have compile-time components itself. *)

module HO $(T : $(X : V) -> sig val w : int end) = struct
  module A = T$(struct let v = 1 end)
end
module Returns $(X : V) = functor (Y : V) -> struct let w = X.v + Y.v end
module Mixed (X : V) = struct module T $(Y : V) = struct let w = X.v + Y.v end end
module Both $(X : V) = struct $let[..] m () = << X.v >> end

(* Subtyping: the body is covariant and the parameter contravariant;
   the two kinds of functor are unrelated. *)

module type BIG = sig val v : int val w : int end
module type SMALL = sig val v : int end
module Wide : $(X : S) -> BIG = functor $(X : S) -> struct let v = 1 let w = 2 end
module Narrow : $(X : S) -> SMALL = Wide
module TakesSmall : $(X : SMALL) -> S = functor $(X : SMALL) -> struct type t end
module TakesBig : $(X : BIG) -> S = TakesSmall
module Nested : $($(X : BIG) -> S) -> S =
  functor $(T : $(X : BIG) -> S) -> struct type t end
module NestedOk : $($(X : SMALL) -> S) -> S = Nested

(* Applications are applicative: their types are those at the path of
   the application, and an alias of the template or of the argument
   is the same path. *)

module M = struct type t = int end
module M' = M
module A = F1$(M)
module B = F1$(M')
module F1' = F1
module C = F1'$(M)
let shared (x : A.t) (y : B.t) (z : C.t) : F1$(M).t list = [x; y; z]
module Generative = F3$()
module Literal = F1$(struct type t = int end)

(* The parameters of both kinds in a path *)
module F7 $(X : S) (Y : S) = struct type t = X.t * Y.t end
module Partial = F7$(M)
module Curried = Partial(M)
let path (x : Curried.t) : F7$(M)(M).t = x

(* Templates in signatures *)
module type H = sig module T : $(X : V) -> sig val w : int end end
module MV = struct let v = 1 end
module MixedMV = Mixed(MV)
module type H' = H with module T = MixedMV.T
module type Of = module type of F6
module rec R : sig module T : $(X : S) -> S end =
  struct module T $(X : S) = struct type t = X.t end end
