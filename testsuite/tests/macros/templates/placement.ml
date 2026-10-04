(* TEST
 expect;
*)

(* A template is a compile-time component wherever it is: a package
   type, a functor argument, a local binding and an abstract module
   type cannot have one. *)

module type S = sig type t end
module type T = $(X : S) -> S
module type Nested = sig module A : sig module B : T end end
;;
[%%expect{|
module type S = sig type t end
module type T = $(X : S) -> S
module type Nested = sig module A : sig module B : T end end
|}]

let f (x : (module T)) = x
;;
[%%expect{|
Line 1, characters 19-20:
1 | let f (x : (module T)) = x
                       ^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
       A package type is read relative to the
       package's own level, so all its members are at that level;
       a component marked for another world has no place in it.
|}]

let f (x : (module Nested)) = x
;;
[%%expect{|
Line 1, characters 19-25:
1 | let f (x : (module Nested)) = x
                       ^^^^^^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
       A package type is read relative to the
       package's own level, so all its members are at that level;
       a component marked for another world has no place in it.
|}]

module F (X : T) = struct end
;;
[%%expect{|
Line 1, characters 9-29:
1 | module F (X : T) = struct end
             ^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Use a template, whose parameter is written "$(X : S)",
       if the argument needs them.
|}]

module type Bad = (X : S) -> sig module A : sig module B : T end end
;;
[%%expect{|
module type Bad = (X : S) -> sig module A : sig module B : T end end
|}]

let f () = let module Local $(X : S) = struct type t = X.t end in 1
;;
[%%expect{|
Line 1, characters 15-62:
1 | let f () = let module Local $(X : S) = struct type t = X.t end in 1
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

module type Abstract = sig module type Inner end
module type Bad = Abstract with module type Inner = T
;;
[%%expect{|
module type Abstract = sig module type Inner end
Line 2, characters 18-53:
2 | module type Bad = Abstract with module type Inner = T
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An abstract module type cannot be instantiated with this module
       type: it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
|}]

(* A template may return a functor; a functor may not take a
   template, but may return one. *)

module type Ok = $(Y : S) -> (X : S) -> S
module type Ok = (X : S) -> $(Y : S) -> S
;;
[%%expect{|
module type Ok = $(Y : S) -> (X : S) -> S
module type Ok = (X : S) -> $(Y : S) -> S
|}]

(* The two kinds of functor are unrelated. *)

module P (X : S) = struct type t = X.t end
module A = P$(struct type t = int end)
;;
[%%expect{|
module P : (X : S) -> sig type t = X.t end
Line 2, characters 11-12:
2 | module A = P$(struct type t = int end)
               ^
Error: This is a functor. It must be applied as "F(M)".
|}]

type u = P$(P).t
;;
[%%expect{|
Line 1, characters 9-10:
1 | type u = P$(P).t
             ^
Error: "P" is a functor. Its application is written "F(M)".
|}]
