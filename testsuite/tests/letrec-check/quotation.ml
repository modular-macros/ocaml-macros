(* TEST
 expect;
*)

(* A quotation builds a value, as a constructor does, and its splices run
   when it is evaluated, whatever the quoted code around them: a
   recursive definition whose quotation splices the variable being
   defined reads it before it is defined. *)

let rec x : (unit -> int) expr = << fun () -> 1 + ($x) () >>
[%%expect{|
Line 1, characters 33-60:
1 | let rec x : (unit -> int) expr = << fun () -> 1 + ($x) () >>
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let v = $(let rec c : int expr = << 1 + $c >> in c)
[%%expect{|
Line 1, characters 33-45:
1 | let v = $(let rec c : int expr = << 1 + $c >> in c)
                                     ^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec f : unit -> int expr = fun () -> << 1 + $(f ()) >>
[%%expect{|
val f : unit -> int expr = <fun>
|}]

let rec l : int expr list = << 1 >> :: l
[%%expect{|
val l : int expr list = [<code 1>; <cycle>]
|}]

(* The quotations of a splice outside a quotation are the code that it
   generates, and are checked as that code. *)

let rec m : int list = 1 :: $(<< m >>)
[%%expect{|
val m : int list = [1; <cycle>]
|}]

let rec n : int list = 1 :: $(<< List.tl n >>)
[%%expect{|
Line 1, characters 23-46:
1 | let rec n : int list = 1 :: $(<< List.tl n >>)
                           ^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
