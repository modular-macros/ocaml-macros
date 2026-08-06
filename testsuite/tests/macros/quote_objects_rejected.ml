(* TEST
 expect;
*)

(* Objects and classes are rejected in staged code -- quotations, macro
   bodies, splice bodies -- with a located error; the method-cache
   bindings Translobj hoists to the enclosing item are not carried into
   either stage.  They work in ordinary run-time code, including
   enclosing a top-level splice (quote_constructors.ml section B). *)

macro q () = << (object method m = 3 end)#m >>
;;

[%%expect{|
Line 1, characters 16-41:
1 | macro q () = << (object method m = 3 end)#m >>
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Objects and classes are not yet supported in quotations,
       or in compile-time code (the body of a macro or of a splice).
|}]

let x = $( ignore (object method m = 3 end); << 1 >> )
;;

[%%expect{|
Line 1, characters 18-43:
1 | let x = $( ignore (object method m = 3 end); << 1 >> )
                      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Objects and classes are not yet supported in quotations,
       or in compile-time code (the body of a macro or of a splice).
|}]
