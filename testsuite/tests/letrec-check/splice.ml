(* TEST
 expect;
*)

let rec a = $(<<a>>)
[%%expect{|
Line 1, characters 12-20:
1 | let rec a = $(<<a>>)
                ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
