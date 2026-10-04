(* TEST
 expect;
*)

(* A generator is bound in compile-time code. *)

module M = struct
  $let[..] g x = << $x + 1 >>
  let w = g
end
;;
[%%expect{|
Line 3, characters 10-11:
3 |   let w = g
              ^
Error: "g" is bound in compile-time code, but this use is at run time.
|}]

module M = struct
  $let[..] g x = << $x + 1 >>
  let c = << g >>
end
;;
[%%expect{|
Line 3, characters 13-14:
3 |   let c = << g >>
                 ^
Error: "g" is bound in compile-time code, but this use is inside a quotation.
       A compile-time value reaches the code a quotation builds only
       through a splice ("$x") or a lift ("Expr.int").
|}]

(* Its body is compile-time code: a run-time value is used there only
   in a quotation. *)

module M = struct
  let w = 1
  $let[..] g () = Expr.int w
end
;;
[%%expect{|
Line 3, characters 27-28:
3 |   $let[..] g () = Expr.int w
                               ^
Error: "w" is bound at run time, but this use is in compile-time code.
|}]

(* A generator is a literal function, and binds one name. *)

module M = struct
  $let[..] g = << 1 >>
end
;;
[%%expect{|
Line 2, characters 15-22:
2 |   $let[..] g = << 1 >>
                   ^^^^^^^
Error: A generator must be defined as a function.
       It is called at compile time, so its definition has to be a
       literal function rather than an expression of function type.
|}]

module M = struct
  $let[..] f x = x
  $let[..] g = f
end
;;
[%%expect{|
Line 3, characters 15-16:
3 |   $let[..] g = f
                   ^
Error: A generator must be defined as a function.
       It is called at compile time, so its definition has to be a
       literal function rather than an expression of function type.
|}]

module M = struct
  $let[..] (f, g) = (fun x -> x), (fun x -> x)
end
;;
[%%expect{|
Line 2, characters 11-17:
2 |   $let[..] (f, g) = (fun x -> x), (fun x -> x)
               ^^^^^^
Error: A generator must bind exactly one name.
       Its compile-time function occupies that name's position in the
       module's compile-time block, so a pattern binding none, or
       several, has nowhere to go.
|}]

(* A static binding may not use a generator. *)

module M = struct
  $let[..] g () = << 1 >>
  $let c = g ()
end
;;
[%%expect{|
Line 3, characters 11-12:
3 |   $let c = g ()
               ^
Error: The generator "g" cannot be used in a static binding ($let):
       the code a generator builds refers to run-time values through
       the environment its caller supplies, and a static binding runs
       with no run-time context to supply one.
       Call it from a splice, or from another generator, instead.
|}]

(* A signature tells generators from values, and from static
   bindings. *)

module M : sig
  val g : int expr -> int expr
end = struct
  $let[..] g x = x
end
;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   $let[..] g x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig $val[..] g : 'a -> 'a end
       is not included in
         sig val g : int expr -> int expr end
       Values do not match:
         $val[..] g : 'a -> 'a
       is not included in
         val g : int expr -> int expr
       One is a generator ($val[..]) and the other is not
|}]

module M : sig
  $val[..] g : int expr -> int expr
end = struct
  let g x = x
end
;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let g x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : 'a -> 'a end
       is not included in
         sig $val[..] g : int expr -> int expr end
       Values do not match:
         val g : 'a -> 'a
       is not included in
         $val[..] g : int expr -> int expr
       One is a generator ($val[..]) and the other is not
|}]

module M : sig
  $val[..] g : int expr -> int expr
end = struct
  $let g x = x
end
;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   $let g x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig $val g : 'a -> 'a end
       is not included in
         sig $val[..] g : int expr -> int expr end
       Values do not match:
         $val g : 'a -> 'a
       is not included in
         $val[..] g : int expr -> int expr
       One is a generator ($val[..]) and the other is not
|}]

module M : sig
  $val g : int expr -> int expr
end = struct
  $let[..] g x = x
end
;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   $let[..] g x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig $val[..] g : 'a -> 'a end
       is not included in
         sig $val g : int expr -> int expr end
       Values do not match:
         $val[..] g : 'a -> 'a
       is not included in
         $val g : int expr -> int expr
       One is a generator ($val[..]) and the other is not
|}]

(* A module with a generator is restricted as one with a static
   binding is. *)

module F (X : sig $val[..] g : int expr -> int expr end) = struct end
;;
[%%expect{|
Line 1, characters 9-69:
1 | module F (X : sig $val[..] g : int expr -> int expr end) = struct end
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The argument of a functor cannot contain compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Use a template, whose parameter is written "$(X : S)",
       if the argument needs them.
|}]

module type S = sig $val[..] g : int expr -> int expr end
let f (m : (module S)) = m
;;
[%%expect{|
module type S = sig $val[..] g : int expr -> int expr end
Line 2, characters 19-20:
2 | let f (m : (module S)) = m
                       ^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators, templates,
       static bindings, or compile-time constructors).
       A package type is read relative to the
       package's own level, so all its members are at that level;
       a component marked for another world has no place in it.
|}]

let f () =
  let module M = struct $let[..] g x = x end in
  ()
;;
[%%expect{|
Line 2, characters 6-44:
2 |   let module M = struct $let[..] g x = x end in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, templates, static bindings,
       or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]
