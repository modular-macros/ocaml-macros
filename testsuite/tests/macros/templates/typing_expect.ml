(* TEST
 expect;
*)

(* Typing of templates, with the phrases evaluated. *)

module type S = sig type t end
module type U = sig type u end
module type BIG = sig val v : int val w : int end
module type SMALL = sig val v : int end
module type WithMacro = sig $val[..] m : int expr -> int expr end
module type WithVal = sig val v : int end
;;

[%%expect{|
module type S = sig type t end
module type U = sig type u end
module type BIG = sig val v : int val w : int end
module type SMALL = sig val v : int end
module type WithMacro = sig $val[..] m : int expr -> int expr end
module type WithVal = sig val v : int end
|}]

(* ------------------------------------------------------------------ *)
(* Ranges: what a functor may contain                                  *)
(* ------------------------------------------------------------------ *)

(* A functor is compiled once, without knowing its argument, so neither
   its argument nor its body may sit below level 0. *)

module type T = (X : S) -> $(Y : S) -> S
;;

[%%expect{|
module type T = (X : S) -> $(Y : S) -> S
|}]

(* The other order is fine: a template may return a functor. *)
module type T = $(Y : S) -> (X : S) -> S
;;

[%%expect{|
module type T = $(Y : S) -> (X : S) -> S
|}]

module F (X : S) = struct $let[..] m x = << $x + 1 >> end
;;

[%%expect{|
module F : (X : S) -> sig $val[..] m : int expr -> int expr end
|}]

module F (X : WithMacro) = struct end
;;

[%%expect{|
Line 1, characters 9-37:
1 | module F (X : WithMacro) = struct end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Use a template, whose parameter is written "$(X : S)",
       if the argument needs them.
|}]

(* A template has no such restriction. *)
module type T = $(X : WithMacro) -> WithMacro
;;

[%%expect{|
module type T = $(X : WithMacro) -> WithMacro
|}]

(* The range is computed recursively, so a level -1 component is
   rejected however deeply it is nested. *)
module type T =
  (X : S) -> sig module A : sig module B : sig $val[..] m : int expr end end end
;;

[%%expect{|
module type T =
  (X : S) ->
    sig module A : sig module B : sig $val[..] m : int expr end end end
|}]

module type T = (X : S) -> sig module A : sig module B : $(Y : S) -> S end end
;;

[%%expect{|
module type T =
  (X : S) -> sig module A : sig module B : $(Y : S) -> S end end
|}]

(* ------------------------------------------------------------------ *)
(* Levels inside a template body                               *)
(* ------------------------------------------------------------------ *)

(* A $let[..] from the argument is usable in a top-level splice. *)
module F$(X : WithMacro) = struct let y = $(X.m << 3 >>) end
;;

[%%expect{|
module F : $(X : WithMacro) -> sig val y : int end
|}]

(* ... but not at level 0. *)
module F$(X : WithMacro) = struct let y = X.m << 3 >> end
;;

[%%expect{|
Line 1, characters 42-45:
1 | module F$(X : WithMacro) = struct let y = X.m << 3 >> end
                                              ^^^
Error: "X.m" is bound in compile-time code, but this use is at run time.
|}]

(* Conversely for an ordinary value component. *)
module F$(X : WithVal) = struct let y = X.v end
;;

[%%expect{|
module F : $(X : WithVal) -> sig val y : int end
|}]

module F$(X : WithVal) = struct let y = $(X.v) end
;;

[%%expect{|
Line 1, characters 42-45:
1 | module F$(X : WithVal) = struct let y = $(X.v) end
                                              ^^^
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
       it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
       A package type is read relative to the
       package's own level, so all its members are at that level;
       a component marked for another world has no place in it.
|}]

module type TF = $(X : S) -> S
let f (x : (module TF)) = x
;;

[%%expect{|
module type TF = $(X : S) -> S
Line 2, characters 19-21:
2 | let f (x : (module TF)) = x
                       ^^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
       A package type is read relative to the
       package's own level, so all its members are at that level;
       a component marked for another world has no place in it.
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
       it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
       A package type is read relative to the
       package's own level, so all its members are at that level;
       a component marked for another world has no place in it.
|}]

let f (x : (module S)) = x
;;

[%%expect{|
val f : (module S) -> (module S) = <fun>
|}]

(* Local module bindings. *)
let f () = let module M = struct $let[..] m x = << $x + 1 >> end in 1
;;

[%%expect{|
Line 1, characters 15-64:
1 | let f () = let module M = struct $let[..] m x = << $x + 1 >> end in 1
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

let f () = let module Local$(X : S) = struct type t = X.t end in 1
;;

[%%expect{|
Line 1, characters 15-61:
1 | let f () = let module Local$(X : S) = struct type t = X.t end in 1
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Bind it at the top level of a structure instead.
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
       type: it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
|}]

module type Bad = Abstract with module type Inner = WithMacro
;;

[%%expect{|
Line 1, characters 18-61:
1 | module type Bad = Abstract with module type Inner = WithMacro
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An abstract module type cannot be instantiated with this module
       type: it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
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
       The module type given for "Inner" has compile-time
       (generator, template, or static binding)
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

(* A type expression may name a component of an application, written
   as the application is: [F(M).t] for a functor, [F$(M).t] for a
   template.  The other spelling is refused where the functor is
   found. *)

module type Tv = sig type t end
module Ft $(X : Tv) = struct type u = X.t end
module Mt = struct type t = int end
module Fp (X : Tv) = struct type u = X.t end
;;

[%%expect{|
module type Tv = sig type t end
module Ft : $(X : Tv) -> sig type u = X.t end
module Mt : sig type t = int end
module Fp : (X : Tv) -> sig type u = X.t end
|}]

type z = Ft$(Mt).u
type y = Fp(Mt).u
;;

[%%expect{|
type z = Ft$(Mt).u
type y = Fp(Mt).u
|}]

type z = Ft(Mt).u
;;

[%%expect{|
Line 1, characters 9-11:
1 | type z = Ft(Mt).u
             ^^
Error: "Ft" is a template. Its application is written "F$(M)".
|}]

type y = Fp$(Mt).u
;;

[%%expect{|
Line 1, characters 9-11:
1 | type y = Fp$(Mt).u
             ^^
Error: "Fp" is a functor. Its application is written "F(M)".
|}]

module At = Ft$(Mt)
type z = At.u
;;

[%%expect{|
module At : sig type u = Mt.t end
type z = At.u
|}]

(* Templates are applicative.  Two applications to one path share
   their abstract types, which are the types at the path of the
   application; an alias of the argument is the same path. *)

module type Ua = sig type u end
module Ga $(X : Ua) : sig type s val mk : unit -> s end =
  struct type s = int let mk () = 0 end
module Ma = struct type u = int end
module Ma' = Ma
module Aa = Ga$(Ma)
module Ba = Ga$(Ma')
let shared (x : Aa.s) : Ba.s = x
let at_path (x : Aa.s) : Ga$(Ma).s = x
;;

[%%expect{|
module type Ua = sig type u end
module Ga : $(X : Ua) -> sig type s val mk : unit -> s end
module Ma : sig type u = int end
module Ma' = Ma
module Aa : sig type s = Ga$(Ma).s val mk : unit -> s end
module Ba : sig type s = Ga$(Ma').s val mk : unit -> s end
val shared : Aa.s -> Ba.s = <fun>
val at_path : Aa.s -> Ga$(Ma).s = <fun>
|}]

(* An argument that is not a path gives the application no path, and
   its abstract types are its own. *)

module Da = Ga$(struct type u = int end)
let distinct (x : Aa.s) : Da.s = x
;;

[%%expect{|
module Da : sig type s val mk : unit -> s end
Line 2, characters 33-34:
2 | let distinct (x : Aa.s) : Da.s = x
                                     ^
Error: The value "x" has type "Aa.s" = "Ga$(Ma).s"
       but an expression was expected of type "Da.s"
|}]

(* A template with a unit parameter is generative, as a functor is. *)

module Ha $() : sig type s val mk : unit -> s end =
  struct type s = int let mk () = 0 end
module Ea = Ha$()
module Fa = Ha$()
let distinct (x : Ea.s) : Fa.s = x
;;

[%%expect{|
module Ha : $() -> sig type s val mk : unit -> s end
module Ea : sig type s val mk : unit -> s end
module Fa : sig type s val mk : unit -> s end
Line 5, characters 33-34:
5 | let distinct (x : Ea.s) : Fa.s = x
                                     ^
Error: The value "x" has type "Ea.s" but an expression was expected of type "Fa.s"
|}]

(* The body of a template obeys the restrictions on the body of an
   applicative functor. *)

module Bad $(X : Ua) = struct module N = Ha$() end
;;

[%%expect{|
Line 1, characters 41-46:
1 | module Bad $(X : Ua) = struct module N = Ha$() end
                                             ^^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}]

module type Pa = sig type t val v : t end
let pa = (module struct type t = int let v = 1 end : Pa)
module Bad $(X : Ua) = struct module N = (val pa : Pa) end
;;

[%%expect{|
module type Pa = sig type t val v : t end
val pa : (module Pa) = <module>
Line 3, characters 41-54:
3 | module Bad $(X : Ua) = struct module N = (val pa : Pa) end
                                             ^^^^^^^^^^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}]

(* A constraint that would make the path of an application ill typed
   is refused. *)

module type Wa = sig
  module M : sig type u end
  type w = Ga$(M).s
end
module type Bad = Wa with type M.u := int
;;

[%%expect{|
module type Wa = sig module M : sig type u end type w = Ga$(M).s end
Line 5, characters 18-41:
5 | module type Bad = Wa with type M.u := int
                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: This "with" constraint on "M.u" makes the applicative functor
       type "Ga$(M).s" ill-typed in the constrained signature:
       Modules do not match: sig end is not included in Ua
       The type "u" is required but not provided
|}]

(* Parameters of both kinds: each application in the path is written
   as its functor is applied. *)

module Mixed $(X : Ua) (Y : Ua) : sig type s val mk : unit -> s end =
  struct type s = int let mk () = 0 end
module Mx = Mixed$(Ma)
module Ia = Mx(Ma)
module Ja = Mx(Ma)
let shared (x : Ia.s) : Ja.s = x
let at_path (x : Ia.s) : Mixed$(Ma)(Ma).s = x
;;

[%%expect{|
module Mixed : $(X : Ua) -> (Y : Ua) -> sig type s val mk : unit -> s end
module Mx : (Y : Ua) -> sig type s = Mixed$(Ma)(Y).s val mk : unit -> s end
module Ia : sig type s = Mixed$(Ma)(Ma).s val mk : unit -> s end
module Ja : sig type s = Mixed$(Ma)(Ma).s val mk : unit -> s end
val shared : Ia.s -> Ja.s = <fun>
val at_path : Ia.s -> Mixed$(Ma)(Ma).s = <fun>
|}]
