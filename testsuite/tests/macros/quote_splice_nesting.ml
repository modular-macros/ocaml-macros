(* TEST
 expect;
*)

(* Quotations and splices each nest only through the other.  Nesting one
   directly in itself is refused; going through the other is accepted. *)

let e = << << 1 >> >>
;;

[%%expect{|
Line 1, characters 11-18:
1 | let e = << << 1 >> >>
               ^^^^^^^
Error: A quotation cannot appear directly inside another quotation.
       Quotations nest only through a splice:
       "<< ... $( ... << ... >> ... ) ... >>".
|}]

$gen m () = << 1 >>
;;

[%%expect{|
$gen m : unit -> int expr = <fun>
|}]

let e = << $( $( m () ) ) >>
;;

[%%expect{|
Line 1, characters 14-23:
1 | let e = << $( $( m () ) ) >>
                  ^^^^^^^^^
Error: A splice cannot appear directly inside another splice.
       Splices nest only through a quotation:
       "$( ... << ... $( ... ) ... >> ... )".
|}]

(* A splice inside a quotation, and a quotation inside that splice. *)

$gen twice () = << $( let inner = << 2 >> in inner ) >>
;;

[%%expect{|
$gen twice : unit -> int expr = <fun>
|}]

let v = $(twice ())
;;

[%%expect{|
val v : int = 2
|}]
