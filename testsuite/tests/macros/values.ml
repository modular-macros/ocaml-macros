(* TEST
 expect;
*)

$gen not_a_function_type = 3
;;

[%%expect{|
Line 1, characters 27-28:
1 | $gen not_a_function_type = 3
                               ^
Error: A generator must be defined as a function.
       It is called at compile time, so its definition has to be a
       literal function rather than an expression of function type.
|}]

$gen also_not_a_function_type = lazy 3
;;

[%%expect{|
Line 1, characters 32-38:
1 | $gen also_not_a_function_type = lazy 3
                                    ^^^^^^
Error: A generator must be defined as a function.
       It is called at compile time, so its definition has to be a
       literal function rather than an expression of function type.
|}]

$gen function_type_but_not_a_value = Fun.id Fun.id
;;

[%%expect{|
Line 1, characters 37-50:
1 | $gen function_type_but_not_a_value = Fun.id Fun.id
                                         ^^^^^^^^^^^^^
Error: A generator must be defined as a function.
       It is called at compile time, so its definition has to be a
       literal function rather than an expression of function type.
|}]

$gen function_fun = fun _ -> 3
;;

[%%expect{|
$gen function_fun : 'a -> int = <fun>
|}]

$gen function_shorthand _ = 3
;;

[%%expect{|
$gen function_shorthand : 'a -> int = <fun>
|}]

$gen function_function = function _ -> 3
;;

[%%expect{|
$gen function_function : 'a -> int = <fun>
|}]

$gen rec a_function = function _ -> 3
and not_a_function = 3
;;

[%%expect{|
Line 2, characters 21-22:
2 | and not_a_function = 3
                         ^
Error: A generator must be defined as a function.
       It is called at compile time, so its definition has to be a
       literal function rather than an expression of function type.
|}]
