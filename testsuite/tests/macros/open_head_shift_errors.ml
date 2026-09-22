(* TEST
 expect;
*)

(* An open serves what its head serves: the refusals.  Each is the
   same refusal the qualified path gets; the accepted shapes are in
   open_head_shift.ml and open_both_worlds.ml. *)

(* A same-unit run-time module opened in a generator body: its value
   is at level 0 there, whichever way it is named.  (Previously the
   open layer took the body's level and the use was ACCEPTED -- a
   generator reading a run-time placeholder.) *)

module L = struct let x = 1 end
$gen g () : int expr = let open L in Expr.int x
;;

[%%expect{|
module L : sig val x : int end
Line 2, characters 46-47:
2 | $gen g () : int expr = let open L in Expr.int x
                                                  ^
Error: "x" is bound at run time, but this use is in compile-time code.
|}]

(* The same module opened at the top of the unit. *)

open L
$gen h () : int expr = Expr.int x
;;

[%%expect{|
Line 2, characters 32-33:
2 | $gen h () : int expr = Expr.int x
                                    ^
Error: "x" is bound at run time, but this use is in compile-time code.
|}]

(* A quotation at the top level of a unit is at +1, where no ordinary
   value is in scope -- through an open no more than by path.
   (Previously the open layer took the quotation's level and the use
   was accepted; the built program then raised an unhandled FreeVar.) *)

let d = << let open L in x >>
;;

[%%expect{|
Line 1, characters 25-26:
1 | let d = << let open L in x >>
                             ^
Error: "x" is bound at run time, but this use is inside a quotation.
|}]

let e = << L.x >>
;;

[%%expect{|
Line 1, characters 11-14:
1 | let e = << L.x >>
               ^^^
Error: "L.x" is bound at run time, but this use is inside a quotation.
|}]
