(* TEST
 expect;
*)

(* Template functors type check; it is the translator that does not yet
   support them.  The syntax itself is exercised by
   tests/parsing/template_functors.ml. *)

module type S = sig type t end
module type WithMacro = sig macro m : int -> int expr end
;;

[%%expect{|
module type S = sig type t end
module type WithMacro = sig macro m : int -> int expr end
|}]

(* Definitions get as far as the translator. *)
module F[X : S] = struct type t = X.t end
;;

[%%expect{|
module F : [X : S] sig type t = X.t end
|}]

module F = functor [X : S] -> struct type t = X.t end
;;

[%%expect{|
module F : [X : S] sig type t = X.t end
|}]

module F[] = struct type t = int end
;;

[%%expect{|
module F : [] sig type t = int end
|}]

(* A template functor type is accepted outright: no code is involved. *)
module type T = [X : S] S
;;

[%%expect{|
module type T = [X : S] S
|}]

module type T = sig module F[X : S] : S end
;;

[%%expect{|
module type T = sig module F : [X : S] S end
|}]

(* Unlike a functor, a template functor may mention macros. *)
module type T = [X : WithMacro] S
;;

[%%expect{|
module type T = [X : WithMacro] S
|}]

(* The two kinds of functor are unrelated, so neither application form
   works on the other kind. *)
module P (X : S) = struct type t = X.t end
;;

[%%expect{|
module P : (X : S) -> sig type t = X.t end
|}]

module A = P[struct type t = int end]
;;

[%%expect{|
Line 1, characters 11-12:
1 | module A = P[struct type t = int end]
               ^
Error: This is an ordinary functor. It must be applied as "F(M)".
|}]

(* Ordinary functors are unaffected. *)
module Q = P(struct type t = int end)
;;

[%%expect{|
module Q : sig type t = int end
|}]
