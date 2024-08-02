(* TEST
 expect;
*)

macro not_a_function_type = 3
;;

[%%expect{|
Line 1, characters 28-29:
1 | macro not_a_function_type = 3
                                ^
Error: macro definition should be a function
|}]

macro also_not_a_function_type = lazy 3
;;

[%%expect{|
Line 1, characters 33-39:
1 | macro also_not_a_function_type = lazy 3
                                     ^^^^^^
Error: macro definition should be a function
|}]

macro function_type_but_not_a_value = Fun.id Fun.id
;;

[%%expect{|
Line 1, characters 38-51:
1 | macro function_type_but_not_a_value = Fun.id Fun.id
                                          ^^^^^^^^^^^^^
Error: macro definition should be a function
|}]

macro function_fun = fun _ -> 3
;;

[%%expect{|
macro function_fun : 'a -> int = <fun>
|}]

macro function_shorthand _ = 3
;;

[%%expect{|
macro function_shorthand : 'a -> int = <fun>
|}]

macro function_function = function _ -> 3
;;

[%%expect{|
macro function_function : 'a -> int = <fun>
|}]

macro rec a_function = function _ -> 3
and not_a_function = 3
;;

[%%expect{|
Line 2, characters 21-22:
2 | and not_a_function = 3
                         ^
Error: macro definition should be a function
|}]
