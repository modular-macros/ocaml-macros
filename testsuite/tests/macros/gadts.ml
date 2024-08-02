(* TEST
 expect;
*)

macro m : type a. (a, int) Type.eq expr -> a -> unit expr =
  fun p x ->
  << match $p with Type.Equal -> $(let _ = 1 + x in << () >>) >>
;;

[%%expect{|
File "_none_", line 1:
Warning 75 [maco-dev]: maco: avoided gadt constraint at level 0

Line 3, characters 47-48:
3 |   << match $p with Type.Equal -> $(let _ = 1 + x in << () >>) >>
                                                   ^
Error: The value "x" has type "a" but an expression was expected of type "int"
|}]

(* TODO: we don't need a warning here *)
macro m : type a. (a, int) Type.eq expr -> unit expr =
  fun p ->
  << match $p with Type.Equal -> () >>
;;

[%%expect{|
File "_none_", line 1:
Warning 75 [maco-dev]: maco: avoided gadt constraint at level 0

macro m : ('a, int) Type.eq expr -> unit expr = <fun>
|}]

(* TODO: we should eventually allow this *)
macro m : type a. (a, int) Type.eq expr -> a expr -> int expr =
 fun p x ->
  << match $p with Type.Equal -> $x >>
;;

[%%expect{|
File "_none_", line 1:
Warning 75 [maco-dev]: maco: avoided gadt constraint at level 0

Line 3, characters 34-35:
3 |   << match $p with Type.Equal -> $x >>
                                      ^
Error: The value "x" has type "a expr" but an expression was expected of type
         "int expr"
       Type "a" is not compatible with type "int"
|}]
