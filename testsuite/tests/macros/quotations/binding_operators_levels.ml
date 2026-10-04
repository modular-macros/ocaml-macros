(* TEST
 expect;
*)

(* A binding operator is a value, used at the level at which it is
   bound, as any other: [let*] bound at run time cannot be used inside a
   quotation or in compile-time code, and one bound in compile-time code
   cannot be used in the code that a quotation builds. *)

let ( let* ) x f = f x
;;
[%%expect{|
val ( let* ) : 'a -> ('a -> 'b) -> 'b = <fun>
|}]

let c = << let* x = 1 in x >>
;;
[%%expect{|
Line 1, characters 11-15:
1 | let c = << let* x = 1 in x >>
               ^^^^
Error: "( let* )" is bound at run time, but this use is inside a quotation.
|}]

let d = $(let* x = << 1 >> in x)
;;
[%%expect{|
Line 1, characters 10-14:
1 | let d = $(let* x = << 1 >> in x)
              ^^^^
Error: "( let* )" is bound at run time, but this use is in compile-time code.
|}]

$let ( let+ ) x f = f x
let e = $(<< let+ x = 1 in x + 1 >>)
;;
[%%expect{|
$val ( let+ ) : 'a -> ('a -> 'b) -> 'b = <fun>
Line 2, characters 13-17:
2 | let e = $(<< let+ x = 1 in x + 1 >>)
                 ^^^^
Error: "(let+)" is bound in compile-time code, but this use is at run time.
       A compile-time value reaches the code a quotation builds only
       through a splice ("$x") or a lift ("Expr.int").
|}]
