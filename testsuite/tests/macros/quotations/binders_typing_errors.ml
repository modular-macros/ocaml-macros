(* TEST
 expect;
*)

(* Every binder in a quotation binds at the level of the quotation: a
   variable it binds is not usable in the compile-time code of a splice
   within its scope. *)

let a = << function (x, _) -> $(ignore x; << 1 >>) >>
;;
[%%expect{|
Line 1, characters 39-40:
1 | let a = << function (x, _) -> $(ignore x; << 1 >>) >>
                                           ^
Error: "x" is bound inside a quotation, but this use is at run time.
|}]

let a = << match Some 1 with Some x as y -> $(ignore y; << 1 >>) | None -> 0 >>
;;
[%%expect{|
Line 1, characters 53-54:
1 | let a = << match Some 1 with Some x as y -> $(ignore y; << 1 >>) | None -> 0 >>
                                                         ^
Error: "y" is bound inside a quotation, but this use is at run time.
|}]

let a = << try 1 with e -> $(ignore e; << 1 >>) >>
;;
[%%expect{|
Line 1, characters 36-37:
1 | let a = << try 1 with e -> $(ignore e; << 1 >>) >>
                                        ^
Error: "e" is bound inside a quotation, but this use is at run time.
|}]

let a = << for i = 1 to 2 do $(ignore i; << () >>) done >>
;;
[%%expect{|
Line 1, characters 38-39:
1 | let a = << for i = 1 to 2 do $(ignore i; << () >>) done >>
                                          ^
Error: "i" is bound inside a quotation, but this use is at run time.
|}]

let a = << match () with () -> 0 | effect _, k -> $(ignore k; << 1 >>) >>
;;
[%%expect{|
Line 1, characters 59-60:
1 | let a = << match () with () -> 0 | effect _, k -> $(ignore k; << 1 >>) >>
                                                               ^
Error: "k" is bound inside a quotation, but this use is at run time.
|}]
