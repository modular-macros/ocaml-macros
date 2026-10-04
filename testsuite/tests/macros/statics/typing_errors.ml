(* TEST
 expect;
*)

(* A static binding is bound in compile-time code, and its right-hand
   side is compile-time code. *)

module M = struct
  $let v = 1
  let w = v
end
;;
[%%expect{|
Line 3, characters 10-11:
3 |   let w = v
              ^
Error: "v" is bound in compile-time code, but this use is at run time.
|}]

module M = struct
  let w = 1
  $let v = w
end
;;
[%%expect{|
Line 3, characters 11-12:
3 |   $let v = w
               ^
Error: "w" is bound at run time, but this use is in compile-time code.
|}]

module M = struct
  $let v = 1
  let c = << v >>
end
;;
[%%expect{|
Line 3, characters 13-14:
3 |   let c = << v >>
                 ^
Error: "v" is bound in compile-time code, but this use is inside a quotation.
       A compile-time value reaches the code a quotation builds only
       through a splice ("$x") or a lift ("Expr.int").
|}]

(* A splice in compile-time code stands in a quotation. *)

module M = struct
  $let v = $(<< 1 >>)
end
;;
[%%expect{|
Line 2, characters 11-21:
2 |   $let v = $(<< 1 >>)
               ^^^^^^^^^^
Error: A splice outside a quotation is evaluated at compile time,
       which is meaningful only at the top level of a module.
       This one is in compile-time code.
|}]

(* A static binding and an ordinary value do not match each other. *)

module M : sig val v : int end = struct
  $let v = 1
end
;;
[%%expect{|
Lines 1-3, characters 33-3:
1 | .................................struct
2 |   $let v = 1
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig $val v : int end
       is not included in
         sig val v : int end
       Values do not match: $val v : int is not included in val v : int
       One is a compile-time binding ($val) and the other an ordinary value
|}]

module M : sig $val v : int end = struct
  let v = 1
end
;;
[%%expect{|
Lines 1-3, characters 34-3:
1 | ..................................struct
2 |   let v = 1
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val v : int end
       is not included in
         sig $val v : int end
       Values do not match: val v : int is not included in $val v : int
       One is a compile-time binding ($val) and the other an ordinary value
|}]

(* Where compile-time components may not go *)

module type S = sig $val v : int end
;;
[%%expect{|
module type S = sig $val v : int end
|}]

module F (X : S) = struct end
;;
[%%expect{|
Line 1, characters 9-29:
1 | module F (X : S) = struct end
             ^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, static bindings, or compile-time constructors).
|}]

module type F = (X : S) -> sig end
;;
[%%expect{|
Line 1, characters 16-34:
1 | module type F = (X : S) -> sig end
                    ^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, static bindings, or compile-time constructors).
|}]

let f (m : (module S)) = m
;;
[%%expect{|
Line 1, characters 19-20:
1 | let f (m : (module S)) = m
                       ^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators,
       static bindings, or compile-time constructors).
|}]

let x = let module M = struct $let v = 1 end in 1
;;
[%%expect{|
Line 1, characters 12-44:
1 | let x = let module M = struct $let v = 1 end in 1
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, static bindings, or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

module type T = sig module type A module X : A end
;;
[%%expect{|
module type T = sig module type A module X : A end
|}]

module type U = T with module type A = S
;;
[%%expect{|
Line 1, characters 16-40:
1 | module type U = T with module type A = S
                    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: An abstract module type cannot be instantiated with this module
       type: it has compile-time components (generators,
       static bindings, or compile-time constructors).
|}]

module X : sig module type A end = struct module type A = S end
;;
[%%expect{|
Line 1, characters 35-63:
1 | module X : sig module type A end = struct module type A = S end
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type A = S end
       is not included in
         sig module type A end
       Module type declarations do not match:
         module type A = S
       does not match
         module type A
       The module type given for "A" has compile-time
       (generator or static binding)
       components, so it cannot match its abstract declaration.
       An abstract module type may only be instantiated with a module type
       whose components all exist at run time.
|}]

(* A module type declared in a signature is not a component. *)

module F (X : sig module type A = S end) = struct end
;;
[%%expect{|
module F : (X : sig module type A = S end) -> sig end
|}]

(* A static binding that a signature hides is a compile-time component
   all the same, of a locally bound module and of a first-class
   module. *)

module type W = sig val w : int end
let f () =
  let module L : W = struct $let s = 1  let w = $(Expr.int s) end in
  L.w
;;
[%%expect{|
module type W = sig val w : int end
Line 3, characters 6-65:
3 |   let module L : W = struct $let s = 1  let w = $(Expr.int s) end in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, static bindings, or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

let p = (module struct $let s = 1  let w = $(Expr.int s) end : W)
;;
[%%expect{|
Line 1, characters 16-60:
1 | let p = (module struct $let s = 1  let w = $(Expr.int s) end : W)
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, static bindings, or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

(* A static binding that an open brings into scope is reported at the
   level at which it serves. *)

module Opened = struct $let s = 1 end
open Opened
let d = s
;;
[%%expect{|
module Opened : sig $val s : int end
Line 3, characters 8-9:
3 | let d = s
            ^
Error: "s" is bound in compile-time code, but this use is at run time.
|}]
