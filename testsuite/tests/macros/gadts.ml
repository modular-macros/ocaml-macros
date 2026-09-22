(* TEST
 expect;
*)

$gen m : type a. (a, int) Type.eq expr -> a -> unit expr =
  fun p x ->
  << match $p with Type.Equal -> $(let _ = 1 + x in << () >>) >>
;;

[%%expect{|
Line 3, characters 47-48:
3 |   << match $p with Type.Equal -> $(let _ = 1 + x in << () >>) >>
                                                   ^
Error: The value "x" has type "a" but an expression was expected of type "int"
|}]

(* A match whose branch needs no equation. *)
$gen m : type a. (a, int) Type.eq expr -> unit expr =
  fun p ->
  << match $p with Type.Equal -> () >>
;;

[%%expect{|
$gen m : ('a, int) Type.eq expr -> unit expr = <fun>
|}]

(* The equation used on the result of a splice in the branch, which
   lands in the branch: allowed (GADTS.md ok2; the full set of cases
   is gadt_staging.ml). *)
$gen m : type a. (a, int) Type.eq expr -> a expr -> int expr =
 fun p x ->
  << match $p with Type.Equal -> $x >>
;;

[%%expect{|
$gen m : ('a, int) Type.eq expr -> 'a expr -> int expr = <fun>
|}]
