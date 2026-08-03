(* TEST
 expect;
*)

macro simple () = << () >>
;;

[%%expect{|
macro simple : unit -> unit expr = <fun>
|}]

macro poly x = << [ $x ] >>
;;

[%%expect{|
macro poly : 'a expr -> 'a list expr = <fun>
|}]

macro sym : type a b. (a, b) Type.eq -> (b, a) Type.eq =
  fun Equal -> Equal
;;

[%%expect{|
macro sym : ('a, 'b) Type.eq -> ('b, 'a) Type.eq = <fun>
|}]

macro rec is_even = function
 | 0 -> true
 | n -> is_odd (n-1)
and is_odd = function
 | 0 -> false
 | n -> is_even (n-1)
;;

[%%expect{|
macro is_even : int -> bool = <fun>
macro is_odd : int -> bool = <fun>
|}]

macro wrong_annotation : int -> int expr =
  fun _ -> << "string" >>
;;

[%%expect{|
Line 2, characters 14-22:
2 |   fun _ -> << "string" >>
                  ^^^^^^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

macro not_recursive : int -> int expr =
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
