(* TEST
 expect;
*)

(* The argument of a functor has no compile-time components, so those
   of its result do not depend on it. *)

module F (X : sig $val[..] g : unit -> int expr end) = struct
  $let[..] h () = X.g ()
end
;;
[%%expect{|
Lines 1-3, characters 9-3:
1 | .........(X : sig $val[..] g : unit -> int expr end) = struct
2 |   $let[..] h () = X.g ()
3 | end
Error: The argument of a functor cannot contain compile-time components
       (generators, static bindings, or compile-time constructors).
|}]

module F (X : sig val v : int end) = struct
  $let s = X.v
end
;;
[%%expect{|
Line 2, characters 11-14:
2 |   $let s = X.v
               ^^^
Error: "X.v" is bound at run time, but this use is in compile-time code.
|}]

(* A functor with compile-time components is not an argument. *)

module G (F : functor (X : sig end) -> sig $val s : int end) = struct end
;;
[%%expect{|
Line 1, characters 9-73:
1 | module G (F : functor (X : sig end) -> sig $val s : int end) = struct end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, static bindings, or compile-time constructors).
|}]

(* A signature tells the compile-time components of the result. *)

module F : functor (X : sig end) -> sig val s : int end =
  functor (X : sig end) -> struct $let s = 1 end
;;
[%%expect{|
Line 2, characters 10-48:
2 |   functor (X : sig end) -> struct $let s = 1 end
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         (X : sig end) -> sig $val s : int end
       is not included in
         (X : sig end) -> sig val s : int end
       Modules do not match:
         sig $val s : int end
       is not included in
         sig val s : int end
       Values do not match: $val s : int is not included in val s : int
       One is a compile-time binding ($val) and the other an ordinary value
|}]
