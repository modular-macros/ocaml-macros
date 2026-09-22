(* TEST
 expect;
*)

(* The refusals of relative pack matching (PACK-MATCHING.md), each
   located; the accepted shapes are in pack_relative.ml. *)

module type S = sig val v : int end
module M = struct let v = 41 end
;;

[%%expect{|
module type S = sig val v : int end
module M : sig val v : int end
|}]

(* A run-time module packed in compile-time code: refused member by
   member by the matching rule, since its slots are placeholders in
   the block the pack reads. *)

$gen a () : int expr = let _p = (module M : S) in << 1 >>
;;

[%%expect{|
Line 1, characters 40-41:
1 | $gen a () : int expr = let _p = (module M : S) in << 1 >>
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

(* A run-time module bound in compile-time code, refused at the
   binding (D-P3), and the same one nested in a literal structure. *)

$gen b () : int expr = let module N = M in Expr.int 0
;;

[%%expect{|
Line 1, characters 38-39:
1 | $gen b () : int expr = let module N = M in Expr.int 0
                                          ^
Error: The module "M" is made at run time, so it cannot be bound
       or packed in compile-time code
       (the body of a generator or of a splice):
       its contents are not available there.
       Use a module from a compile-time library
       (a directory given to "-I-static"), or unpack a compile-time package.
|}]

$gen c () : int expr =
  let module Q = struct module A = M end in Expr.int 0
;;

[%%expect{|
Line 2, characters 35-36:
2 |   let module Q = struct module A = M end in Expr.int 0
                                       ^
Error: The module "M" is made at run time, so it cannot be bound
       or packed in compile-time code
       (the body of a generator or of a splice):
       its contents are not available there.
       Use a module from a compile-time library
       (a directory given to "-I-static"), or unpack a compile-time package.
|}]

(* A functor member of a package built in compile-time code: not yet. *)

module type FS = sig module F : functor (X : sig end) -> sig end end
$gen d () : int expr = let _p = (module Stdlib : FS) in << 1 >>
;;

[%%expect{|
module type FS = sig module F : (X : sig end) -> sig end end
Line 2, characters 40-46:
2 | $gen d () : int expr = let _p = (module Stdlib : FS) in << 1 >>
                                            ^^^^^^
Error: This package type has a functor member, "F".
       Packing a functor in compile-time code
       (the body of a generator or of a splice) is not yet supported.
|}]

(* A functor application is not admissible in compile-time code. *)

module F (X : sig end) = struct let v = 1 end
$gen e () : int expr = let module N = F (struct end) in Expr.int 0
;;

[%%expect{|
module F : (X : sig end) -> sig val v : int end
Line 2, characters 38-52:
2 | $gen e () : int expr = let module N = F (struct end) in Expr.int 0
                                          ^^^^^^^^^^^^^^
Error: This module expression cannot be evaluated in compile-time
       code (the body of a generator or of a splice):
       only a module from a compile-time library,
       an unpacked compile-time package, or a literal structure can be.
|}]

(* Inside a compile-time literal structure a static or a generator
   would sit one further level down. *)

$gen f () : int expr = let module Q = struct $let z = 1 end in << 1 >>
;;

[%%expect{|
Line 1, characters 45-55:
1 | $gen f () : int expr = let module Q = struct $let z = 1 end in << 1 >>
                                                 ^^^^^^^^^^
Error: A module defined in compile-time code (the body of a generator
       or of a splice) cannot define generators or static bindings:
       they would sit one further level down.
       Define them at the top level of a structure instead.
|}]

(* What now works, in the toplevel too: a literal structure with
   values in compile-time code, packed relatively and unpacked. *)

module type Z = sig val zero : int end
$gen g () : int expr =
  let p = (module (struct let zero = 7 end) : Z) in
  let module N = (val p) in
  Expr.int (N.zero + 1)
;;
let r = $(g ())
;;

[%%expect{|
module type Z = sig val zero : int end
$gen g : unit -> int expr = <fun>
val r : int = 8
|}]

(* A pack passed to a DEPENDENT package parameter carries its
   annotation into the package's own coercion, so a run-time module is
   still refused there -- by the relative rule, not by the absolute
   one (pack_relative_dependent.ml). *)

module type Sd = sig type _ t val v : int end
$gen used : (module M : Sd) -> 'a M.t -> int expr =
  fun (module M : Sd) _x -> Expr.int M.v
module Run = struct type 'a t = 'a list  let v = 41 end
;;
let bad = $(used (module Run : Sd) [1])
;;

[%%expect{|
module type Sd = sig type _ t val v : int end
$gen used : (module M : Sd) -> 'a M.t -> int expr = <fun>
module Run : sig type 'a t = 'a list val v : int end
Line 6, characters 25-28:
6 | let bad = $(used (module Run : Sd) [1])
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
