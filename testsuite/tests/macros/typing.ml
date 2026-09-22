(* TEST
 expect;
*)

$gen simple () = << () >>
;;

[%%expect{|
$gen simple : unit -> unit expr = <fun>
|}]

$gen poly x = << [ $x ] >>
;;

[%%expect{|
$gen poly : 'a expr -> 'a list expr = <fun>
|}]

$gen sym : type a b. (a, b) Type.eq -> (b, a) Type.eq =
  fun Equal -> Equal
;;

[%%expect{|
$gen sym : ('a, 'b) Type.eq -> ('b, 'a) Type.eq = <fun>
|}]

$gen rec is_even = function
 | 0 -> true
 | n -> is_odd (n-1)
and is_odd = function
 | 0 -> false
 | n -> is_even (n-1)
;;

[%%expect{|
$gen is_even : int -> bool = <fun>
$gen is_odd : int -> bool = <fun>
|}]

$gen wrong_annotation : int -> int expr =
  fun _ -> << "string" >>
;;

[%%expect{|
Line 2, characters 14-22:
2 |   fun _ -> << "string" >>
                  ^^^^^^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

$gen not_recursive : int -> int expr =
  fun _ -> << 0 >>
and also_not_recursive : int -> int expr =
  fun x -> not_recursive x
;;

[%%expect{|
Line 4, characters 11-24:
4 |   fun x -> not_recursive x
               ^^^^^^^^^^^^^
Error: Unbound value "not_recursive"
Hint: If this is a recursive definition,
you should add the "rec" keyword on line 1
|}]
