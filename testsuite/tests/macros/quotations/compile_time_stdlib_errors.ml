(* TEST
 expect;
*)

(* A quotation outside compile-time code is one level above the
   standard library. *)

let a = << List.length [] >>
;;
[%%expect{|
Line 1, characters 11-22:
1 | let a = << List.length [] >>
               ^^^^^^^^^^^
Error: "List.length" is bound at run time, but this use is inside a quotation.
|}]

let a = << let open List in length [] >>
;;
[%%expect{|
Line 1, characters 28-34:
1 | let a = << let open List in length [] >>
                                ^^^^^^
Error: "length" is bound at run time, but this use is inside a quotation.
|}]

(* A module of the program serves run-time code only. *)

module M = struct let x = 1 end
;;
[%%expect{|
module M : sig val x : int end
|}]

let a = $(ignore M.x; << 1 >>)
;;
[%%expect{|
Line 1, characters 17-20:
1 | let a = $(ignore M.x; << 1 >>)
                     ^^^
Error: "M.x" is bound at run time, but this use is in compile-time code.
|}]

let a = $(let open M in ignore x; << 1 >>)
;;
[%%expect{|
Line 1, characters 31-32:
1 | let a = $(let open M in ignore x; << 1 >>)
                                   ^
Error: "x" is bound at run time, but this use is in compile-time code.
|}]

open M
;;
[%%expect{|
|}]

let a = $(ignore x; << 1 >>)
;;
[%%expect{|
Line 1, characters 17-18:
1 | let a = $(ignore x; << 1 >>)
                     ^
Error: "x" is bound at run time, but this use is in compile-time code.
|}]
