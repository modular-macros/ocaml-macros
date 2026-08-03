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

macro m () = << 1 >>
;;

[%%expect{|
macro m : unit -> int expr = <fun>
|}]

let e = << $( $( m () ) ) >>
;;

[%%expect{|
Line 1, characters 12-25:
1 | let e = << $( $( m () ) ) >>
                ^^^^^^^^^^^^^
Error: A splice cannot appear directly inside another splice.
       Splices nest only through a quotation:
       "$( ... << ... $( ... ) ... >> ... )".
|}]

(* A splice inside a quotation, and a quotation inside that splice. *)

macro twice () = << $( let inner = << 2 >> in inner ) >>
;;

[%%expect{|
macro twice : unit -> int expr = <fun>
|}]

let v = $(twice ())
;;

[%%expect{|
val v : int = 2
|}]
