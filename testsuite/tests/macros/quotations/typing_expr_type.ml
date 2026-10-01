(* TEST
 expect;
*)

(* The type expr: a quotation is as expansive as the code it builds, the
   type is covariant, and a type of the program may shadow its name. *)

let a = << ref [] >>
let b = << fun () -> ref [] >>
;;
[%%expect{|
val a : '_weak1 list ref expr = <code (makemutable 0 0)>
val b : (unit -> 'a list ref) expr =
  <code (function param (makemutable 0 0))>
|}]

let c (x : < m : int; n : int > expr) = (x :> < m : int > expr)
;;
[%%expect{|
val c : < m : int; n : int > expr -> < m : int > expr = <fun>
|}]

type expr = Int of int
let d (e : expr) = e
let e = d << 1 >>
;;
[%%expect{|
type expr = Int of int
val d : expr -> expr = <fun>
Line 3, characters 10-17:
3 | let e = d << 1 >>
              ^^^^^^^
Error: This expression has type "'a expr/2"
       but an expression was expected of type "expr"
       Line 1, characters 0-22:
         Definition of type "expr"
       File "_none_", line 1:
         Definition of type "expr/2"
|}]
