(* TEST
 expect;
*)

(* Typing of template functors.  The syntax is exercised by
   tests/parsing/template_functors.ml, and the fact that the translator
   does not yet handle them by tests/macros/template_functors.ml. *)

module type S = sig type t end
module type U = sig type u end
module type BIG = sig val v : int val w : int end
module type SMALL = sig val v : int end
module type WithMacro = sig macro m : int expr -> int expr end
module type WithVal = sig val v : int end
;;

[%%expect{|
module type S = sig type t end
module type U = sig type u end
module type BIG = sig val v : int val w : int end
module type SMALL = sig val v : int end
module type WithMacro = sig macro m : int expr -> int expr end
module type WithVal = sig val v : int end
|}]

(* ------------------------------------------------------------------ *)
(* Ranges: what a functor may contain                                  *)
(* ------------------------------------------------------------------ *)

(* A functor is compiled once, without knowing its argument, so neither
   its argument nor its body may sit below level 0. *)

module type T = (X : S) -> [Y : S] S
;;

[%%expect{|
module type T = (X : S) -> [Y : S] S
|}]

(* The other order is fine: a template functor may return a functor. *)
module type T = [Y : S] (X : S) -> S
;;

[%%expect{|
module type T = [Y : S] (X : S) -> S
|}]

module F (X : S) = struct macro m x = << $x + 1 >> end
;;

[%%expect{|
module F : (X : S) -> sig macro m : int expr -> int expr end
|}]

module F (X : WithMacro) = struct end
;;

[%%expect{|
Line 1, characters 9-37:
1 | module F (X : WithMacro) = struct end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain macros or template functors.
       Use a template functor, whose parameter is written "[X : S]",
       if the argument needs them.
|}]

(* A template functor has no such restriction. *)
module type T = [X : WithMacro] WithMacro
;;

[%%expect{|
module type T = [X : WithMacro] WithMacro
|}]

(* The range is computed recursively, so a level -1 component is
   rejected however deeply it is nested. *)
module type T =
  (X : S) -> sig module A : sig module B : sig macro m : int expr end end end
;;

[%%expect{|
module type T =
  (X : S) -> sig module A : sig module B : sig macro m : int expr end end end
|}]

module type T = (X : S) -> sig module A : sig module B : [Y : S] S end end
;;

[%%expect{|
module type T = (X : S) -> sig module A : sig module B : [Y : S] S end end
|}]

(* ------------------------------------------------------------------ *)
(* Levels inside a template functor body                               *)
(* ------------------------------------------------------------------ *)

(* A macro from the argument is usable in a top-level splice. *)
module F[X : WithMacro] = struct let y = $(X.m << 3 >>) end
;;

[%%expect{|
module F : [X : WithMacro] sig val y : int end
|}]

(* ... but not at level 0. *)
module F[X : WithMacro] = struct let y = X.m << 3 >> end
;;

[%%expect{|
Line 1, characters 41-44:
1 | module F[X : WithMacro] = struct let y = X.m << 3 >> end
                                             ^^^
Error: "X.m" is bound in compile-time code, but this use is at run time.
|}]

(* Conversely for an ordinary value component. *)
module F[X : WithVal] = struct let y = X.v end
;;

[%%expect{|
module F : [X : WithVal] sig val y : int end
|}]

module F[X : WithVal] = struct let y = $(X.v) end
;;

[%%expect{|
Line 1, characters 40-45:
1 | module F[X : WithVal] = struct let y = $(X.v) end
                                            ^^^^^
Error: "X.v" is bound at run time, but this use is in compile-time code.
|}]

(* ------------------------------------------------------------------ *)
(* Level -1 components are confined to top-level module bindings       *)
(* ------------------------------------------------------------------ *)

(* First-class modules. *)
let f (x : (module WithMacro)) = x
;;

[%%expect{|
Line 1, characters 19-28:
1 | let f (x : (module WithMacro)) = x
                       ^^^^^^^^^
Error: This module type cannot be used as a first-class module:
       it has macro or template functor components,
       which exist only at compile time.
|}]

module type TF = [X : S] S
let f (x : (module TF)) = x
;;

[%%expect{|
module type TF = [X : S] S
Line 2, characters 19-21:
2 | let f (x : (module TF)) = x
                       ^^
Error: This module type cannot be used as a first-class module:
       it has macro or template functor components,
       which exist only at compile time.
|}]

module type Nested = sig module A : sig module B : WithMacro end end
let f (x : (module Nested)) = x
;;

[%%expect{|
module type Nested = sig module A : sig module B : WithMacro end end
Line 2, characters 19-25:
2 | let f (x : (module Nested)) = x
                       ^^^^^^
Error: This module type cannot be used as a first-class module:
       it has macro or template functor components,
       which exist only at compile time.
|}]

let f (x : (module S)) = x
;;

[%%expect{|
val f : (module S) -> (module S) = <fun>
|}]

(* Local module bindings. *)
let f () = let module M = struct macro m x = << $x + 1 >> end in 1
;;

[%%expect{|
Line 1, characters 15-61:
1 | let f () = let module M = struct macro m x = << $x + 1 >> end in 1
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have macro or template functor
       components. Bind it at the top level of a structure instead.
|}]

let f () = let module Local[X : S] = struct type t = X.t end in 1
;;

[%%expect{|
Line 1, characters 15-60:
1 | let f () = let module Local[X : S] = struct type t = X.t end in 1
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have macro or template functor
       components. Bind it at the top level of a structure instead.
|}]

let f () = let module M = struct let v = 1 end in M.v
;;

[%%expect{|
val f : unit -> int = <fun>
|}]

(* Instantiating an abstract module type. *)
module type Abstract = sig module type Inner end
;;

[%%expect{|
module type Abstract = sig module type Inner end
|}]

module type Bad = Abstract with module type Inner = TF
;;

[%%expect{|
Line 1, characters 18-54:
1 | module type Bad = Abstract with module type Inner = TF
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An abstract module type cannot be instantiated with this module
       type: it has macro or template functor components.
|}]

module type Bad = Abstract with module type Inner = WithMacro
;;

[%%expect{|
Line 1, characters 18-61:
1 | module type Bad = Abstract with module type Inner = WithMacro
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An abstract module type cannot be instantiated with this module
       type: it has macro or template functor components.
|}]

module type Ok = Abstract with module type Inner = S
;;

[%%expect{|
module type Ok = sig module type Inner = S end
|}]

(* The other route to the same restriction is signature matching, which
   reports it as an ordinary inclusion mismatch, with its context.  The
   functor-application form of this is
   tests/macros/abstract_modtype_macro_instantiation.ml. *)
module Direct : Abstract = struct module type Inner = WithMacro end
;;

[%%expect{|
Line 1, characters 27-67:
1 | module Direct : Abstract = struct module type Inner = WithMacro end
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type Inner = WithMacro end
       is not included in
         Abstract
       Module type declarations do not match:
         module type Inner = WithMacro
       does not match
         module type Inner
       The module type given for "Inner" has macro or template functor
       components, so it cannot match its abstract declaration.
       An abstract module type may only be instantiated with a module type
       whose components all exist at run time.
|}]

module Direct : Abstract = struct module type Inner = S end
;;

[%%expect{|
module Direct : Abstract
|}]

(* ------------------------------------------------------------------ *)
(* Applications in type expressions                                    *)
(* ------------------------------------------------------------------ *)

(* A type expression may name a component of an applicative functor
   application, [F(M).t].  A template functor's application is compile-time
   evaluation, not a path, so there is no such component to name -- and the
   syntax cannot say which of the two is meant either, [F(M)] being the
   ordinary application form.  Path resolution therefore refuses it where
   it finds the functor, naming it.  The way to write this is to bind the
   application first, [module A = F[M]], and name [A.u]. *)

module type Tv = sig type t end
module Ft [X : Tv] = struct type u = X.t end
module Mt = struct type t = int end
;;

[%%expect{|
module type Tv = sig type t end
module Ft : [X : Tv] sig type u = X.t end
module Mt : sig type t = int end
|}]

type z = Ft(Mt).u
;;

[%%expect{|
Line 1, characters 9-11:
1 | type z = Ft(Mt).u
             ^^
Error: "Ft" is a template functor, so it cannot be applied in a type
       expression.  A template functor is applied at compile time, and
       its result is a fresh module each time.
|}]

module At = Ft[Mt]
type z = At.u
;;

[%%expect{|
module At : sig type u = Mt.t end
type z = At.u
|}]
