(* TEST
 expect;
*)

(* A module made at run time has only placeholders in compile-time
   code: compile-time code cannot bind or pack it. *)

module R = struct let v = 1 end
module type S = sig val v : int end
;;
[%%expect{|
module R : sig val v : int end
module type S = sig val v : int end
|}]

$let x = let module M = R in M.v
;;
[%%expect{|
Line 1, characters 29-32:
1 | $let x = let module M = R in M.v
                                 ^^^
Error: "M.v" is bound at run time, but this use is in compile-time code.
|}]

$let p = (module R : S)
;;
[%%expect{|
Line 1, characters 17-18:
1 | $let p = (module R : S)
                     ^
Error: Signature mismatch:
       Modules do not match: sig val v : int end is not included in S
       Values do not match: val v : int is not included in val v : int
       The first is made at run time, but this package is built in
       compile-time code (the body of a generator or of a splice),
       which holds only compile-time values:
       static bindings, or the contents of a module from a
       compile-time library (a directory given to -I-static)
|}]

$let[..] g () = let module M = struct module N = R end in Expr.int 0
;;
[%%expect{|
Line 1, characters 49-50:
1 | $let[..] g () = let module M = struct module N = R end in Expr.int 0
                                                     ^
Error: The module "R" is made at run time, so it cannot be bound
       or packed in compile-time code
       (the body of a generator or of a splice):
       its contents are not available there.
       Use a module from a compile-time library
       (a directory given to "-I-static"), or unpack a compile-time package.
|}]

(* A package type reads all its members at the level of the package. *)

module type T = sig $val v : int end
module M = struct $let v = 1 end
;;
[%%expect{|
module type T = sig $val v : int end
module M : sig $val v : int end
|}]

$let p = (module M : T)
;;
[%%expect{|
Line 1, characters 21-22:
1 | $let p = (module M : T)
                         ^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators,
       static bindings, or compile-time constructors).
|}]

let p = (module M : S)
;;
[%%expect{|
Line 1, characters 16-17:
1 | let p = (module M : S)
                    ^
Error: Signature mismatch:
       Modules do not match: sig $val v : int end is not included in S
       Values do not match: $val v : int is not included in val v : int
       One is a compile-time binding ($val) and the other an ordinary value
|}]

(* The code that a quotation builds cannot name a module bound in
   compile-time code. *)

$let[..] g () = let module M = struct let v = 1 end in << M.v >>
;;
[%%expect{|
Line 1, characters 58-61:
1 | $let[..] g () = let module M = struct let v = 1 end in << M.v >>
                                                              ^^^
Error: The module "M" is bound in compile-time code, so the code this
       quotation builds cannot name it: that code runs in another
       program, which has no such module.
       Bind the module outside the generator or splice body, or move the
       reference into a splice.
|}]

$let[..] g (module M : S) = << M.v >>
;;
[%%expect{|
Line 1, characters 31-34:
1 | $let[..] g (module M : S) = << M.v >>
                                   ^^^
Error: The module "M" is bound in compile-time code, so the code this
       quotation builds cannot name it: that code runs in another
       program, which has no such module.
       Bind the module outside the generator or splice body, or move the
       reference into a splice.
|}]

(* Compile-time code cannot define compile-time components, nor
   classes. *)

$let x = let module M = struct $let v = 1 end in 0
;;
[%%expect{|
Line 1, characters 31-41:
1 | $let x = let module M = struct $let v = 1 end in 0
                                   ^^^^^^^^^^
Error: A module defined in compile-time code (the body of a generator
       or of a splice) cannot define generators, static bindings
       or compile-time constructors: they would sit one further level down.
       Define them at the top level of a structure instead.
|}]

$let x = let module M = struct class c = object end end in 0
;;
[%%expect{|
Line 1, characters 31-51:
1 | $let x = let module M = struct class c = object end end in 0
                                   ^^^^^^^^^^^^^^^^^^^^
Error: Objects and classes are not yet supported in quotations,
       or in compile-time code.
|}]

(* A package built in compile-time code cannot have functor members. *)

module type FS = sig module F : functor (X : sig end) -> sig end end
$let p = (module struct module F (X : sig end) = struct end end : FS)
;;
[%%expect{|
module type FS = sig module F : (X : sig end) -> sig end end
Line 2, characters 33-59:
2 | $let p = (module struct module F (X : sig end) = struct end end : FS)
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module expression cannot be evaluated in compile-time
       code (the body of a generator or of a splice):
       only a module from a compile-time library,
       an unpacked compile-time package, or a literal structure can be.
|}]

(* A package passed to a dependent parameter is read at the level of
   the package too. *)

module type Sd = sig type _ t val v : int end
$let[..] used : (module M : Sd) -> 'a M.t -> int expr =
  fun (module M : Sd) _ -> Expr.int M.v
module Run = struct type 'a t = 'a list  let v = 41 end
let bad = $(used (module Run : Sd) [1])
;;
[%%expect{|
module type Sd = sig type _ t val v : int end
$val[..] used : (module M : Sd) -> 'a M.t -> int expr = <fun>
module Run : sig type 'a t = 'a list val v : int end
Line 5, characters 25-28:
5 | let bad = $(used (module Run : Sd) [1])
                             ^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a list val v : int end
       is not included in
         sig type _ t val v : int end
       Values do not match: val v : int is not included in val v : int
       The first is made at run time, but this package is built in
       compile-time code (the body of a generator or of a splice),
       which holds only compile-time values:
       static bindings, or the contents of a module from a
       compile-time library (a directory given to -I-static)
|}]
