(* TEST
 expect;
*)

(* Quotations and splices nest only through each other. *)

let a = << << 1 >> >>
;;
[%%expect{|
Line 1, characters 11-18:
1 | let a = << << 1 >> >>
               ^^^^^^^
Error: A quotation cannot appear directly inside another quotation.
       Quotations nest only through a splice:
       "<< ... $( ... << ... >> ... ) ... >>".
|}]

let a = $($(<< << 1 >> >>))
;;
[%%expect{|
Line 1, characters 10-26:
1 | let a = $($(<< << 1 >> >>))
              ^^^^^^^^^^^^^^^^
Error: A splice cannot appear directly inside another splice.
       Splices nest only through a quotation:
       "$( ... << ... $( ... ) ... >> ... )".
|}]

(* A value is used only at the level at which it is bound. *)

let x = 1
;;
[%%expect{|
val x : int = 1
|}]

let a = << x >>
;;
[%%expect{|
Line 1, characters 11-12:
1 | let a = << x >>
               ^
Error: "x" is bound at run time, but this use is inside a quotation.
|}]

let a = $(ignore x; << 1 >>)
;;
[%%expect{|
Line 1, characters 17-18:
1 | let a = $(ignore x; << 1 >>)
                     ^
Error: "x" is bound at run time, but this use is in compile-time code.
|}]

let a = $(let y = 1 in << y >>)
;;
[%%expect{|
Line 1, characters 26-27:
1 | let a = $(let y = 1 in << y >>)
                              ^
Error: "y" is bound in compile-time code, but this use is at run time.
|}]

let a = << fun y -> $(y) >>
;;
[%%expect{|
Line 1, characters 22-23:
1 | let a = << fun y -> $(y) >>
                          ^
Error: "y" is bound inside a quotation, but this use is at run time.
|}]

(* The standard library is bound at run time. *)

let a = << print_newline () >>
;;
[%%expect{|
Line 1, characters 11-24:
1 | let a = << print_newline () >>
               ^^^^^^^^^^^^^
Error: "print_newline" is bound at run time, but this use is inside a quotation.
|}]

(* The types of quotations and splices *)

let a : int expr = << "a" >>
;;
[%%expect{|
Line 1, characters 22-25:
1 | let a : int expr = << "a" >>
                          ^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

let a : int = $(<< "a" >>)
;;
[%%expect{|
Line 1, characters 19-22:
1 | let a : int = $(<< "a" >>)
                       ^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

let a = $(1)
;;
[%%expect{|
Line 1, characters 10-11:
1 | let a = $(1)
              ^
Error: The constant "1" has type "int" but an expression was expected of type
         "'a expr"
|}]

let a = << 1 >> + 1
;;
[%%expect{|
Line 1, characters 8-15:
1 | let a = << 1 >> + 1
            ^^^^^^^
Error: This expression has type "'a expr"
       but an expression was expected of type "int"
|}]
