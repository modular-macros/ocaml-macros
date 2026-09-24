(* TEST
 expect;
*)

(* A package is built from a module's run-time contents, so in
   compile-time code (the body of a $gen or of a splice) only a
   module served at compile time -- one from a unit registered with
   -I-static, or the stdlib, registered for both worlds by default --
   can be packed.  The compiling unit's own modules exist in that
   world only as placeholders; packing one used to build the package
   FROM the placeholder, baking zeros into the generated code with no
   error anywhere, and repacking a locally rebound module crashed the
   static program.  A package whose type names no run-time contents
   is inert and stays allowed, as does a pack inside a quotation,
   which is run-time code. *)

module type S = sig val v : int end
module M = struct type t = int let v = 41 end
;;

[%%expect{|
module type S = sig val v : int end
module M : sig type t = int val v : int end
|}]

$gen m () : int expr = let _p = (module M : S) in << 1 >>
;;

[%%expect{|
Line 1, characters 40-41:
1 | $gen m () : int expr = let _p = (module M : S) in << 1 >>
                                            ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = int val v : int end
       is not included in
         S
       Values do not match: val v : int is not included in val v : int
       The first is made at run time, but this package is built in
       compile-time code (the body of a generator or of a splice),
       which holds only compile-time values:
       static bindings, or the contents of a module from a
       compile-time library (a directory given to -I-static)
|}]

(* Laundering through a local binding is caught at the BINDING now:
   a run-time module cannot be bound in compile-time code (D-P3). *)

$gen m () : int expr =
  let module N = M in let _p = (module N : S) in << 1 >>
;;

[%%expect{|
Line 2, characters 17-18:
2 |   let module N = M in let _p = (module N : S) in << 1 >>
                     ^
Error: The module "M" is made at run time, so it cannot be bound
       or packed in compile-time code
       (the body of a generator or of a splice):
       its contents are not available there.
       Use a module from a compile-time library
       (a directory given to "-I-static"), or unpack a compile-time package.
|}]

(* The stdlib is served at compile time, so its units pack. *)

module type Z = sig val zero : int end
$gen ok () : int expr = let _p = (module Stdlib.Int : Z) in << 8 >>
;;
let a = $(ok ())
;;

[%%expect{|
module type Z = sig val zero : int end
$gen ok : unit -> int expr = <fun>
val a : int = 8
|}]

(* ...but the served-at-compile-time property does not travel through
   a local rebinding of an unpack. *)

$gen r () : int expr =
  let p = (module Stdlib.Int : Z) in
  let module N = (val p) in
  let _q = (module N : Z) in << 9 >>
;;

[%%expect{|
$gen r : unit -> int expr = <fun>
|}]

(* A package of types alone carries no run-time contents: inert,
   allowed. *)

module type TO = sig type t end
$gen t () : int expr = let _p = (module M : TO) in << 10 >>
;;
let b = $(t ())
;;

[%%expect{|
module type TO = sig type t end
$gen t : unit -> int expr = <fun>
val b : int = 10
|}]

(* Inside a quotation the pack is run-time code and stays allowed. *)

$gen q () : int expr = << let module X = (val (module M : S)) in X.v >>
;;
let c = $(q ())
;;

[%%expect{|
$gen q : unit -> int expr = <fun>
val c : int = 41
|}]
