(* TEST
 expect;
*)

(* A quotation may call a method of an object that it is given, but may
   not make an object: [new], an object expression and the forms that
   only an object's methods can use are refused. *)

let a = << fun o -> o#m + 1 >>
;;
[%%expect{|
val a : (< m : int; .. > -> int) expr =
  <code (function o (+ (send o 109) 1))>
|}]

class c = object method m = 1 end
let b = << new c >>
;;
[%%expect{|
class c : object method m : int end
Line 2, characters 11-16:
2 | let b = << new c >>
               ^^^^^
Error: Objects and classes are not yet supported in quotations,
       or in compile-time code.
|}]

let d = $(<< (new c)#m >>)
;;
[%%expect{|
Line 1, characters 13-20:
1 | let d = $(<< (new c)#m >>)
                 ^^^^^^^
Error: Objects and classes are not yet supported in quotations,
       or in compile-time code.
|}]
