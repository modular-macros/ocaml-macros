(* TEST
 expect;
*)

(* A generator attaches its run-time environment at the REFERENCE
   site, so the code it builds can only be handed to the splice that
   called it (a fixed run-time world) or to another generator (which
   threads its own).  A static binding evaluates once in the compile
   world and its value may travel to any consumer: it has no run-time
   context to thread, so generator references inside its initialiser
   are refused -- the effect model of STATIC-BINDINGS.md, in which
   capturing future names is an effect $gen is pure for, $let is
   impure for, and a splice handles.  Before the guard, the shape at
   the end of this file (a generator called from a $let, under a
   narrowing signature) left the environment access unbound in the
   emitted code: a Bytegen fatal error. *)

let secret = 100
$gen g () = << secret >>
;;

[%%expect{|
val secret : int = 100
$gen g : unit -> int expr = <fun>
|}]

(* The direct call. *)

$let f () = g ()
;;

[%%expect{|
Line 1, characters 12-13:
1 | $let f () = g ()
                ^
Error: The generator "g" cannot be used in a static binding ($let):
       the code a generator builds refers to run-time values through
       the environment its caller supplies, and a static binding runs
       with no run-time context to supply one.
       Call it from a splice, or from another generator, instead.
|}]

(* A first-class reference is the same thing: the environment is
   attached where the generator is NAMED. *)

$let h = g
;;

[%%expect{|
Line 1, characters 9-10:
1 | $let h = g
             ^
Error: The generator "g" cannot be used in a static binding ($let):
       the code a generator builds refers to run-time values through
       the environment its caller supplies, and a static binding runs
       with no run-time context to supply one.
       Call it from a splice, or from another generator, instead.
|}]

(* ...and so is a reference from an antiquotation inside the
   initialiser's quotation. *)

$let q = << $(g ()) + 1 >>
;;

[%%expect{|
Line 1, characters 14-15:
1 | $let q = << $(g ()) + 1 >>
                  ^
Error: The generator "g" cannot be used in a static binding ($let):
       the code a generator builds refers to run-time values through
       the environment its caller supplies, and a static binding runs
       with no run-time context to supply one.
       Call it from a splice, or from another generator, instead.
|}]

(* What IS allowed: a static function may receive a generator's value
   from a splice, where the reference site supplied the environment,
   and call it. *)

$let apply k = k ()
;;
let v = $(apply g)
;;

[%%expect{|
$val apply : (unit -> 'a) -> 'a = <fun>
val v : int = 100
|}]

(* The reported shape: same unit, generator hidden by the signature. *)

module N : sig $val f : unit -> int expr end = struct
  let secret = 100
  $gen m () = << secret >>
  $let f () = m ()
end
;;

[%%expect{|
Line 4, characters 14-15:
4 |   $let f () = m ()
                  ^
Error: The generator "m" cannot be used in a static binding ($let):
       the code a generator builds refers to run-time values through
       the environment its caller supplies, and a static binding runs
       with no run-time context to supply one.
       Call it from a splice, or from another generator, instead.
|}]
