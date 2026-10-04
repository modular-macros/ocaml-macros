(* TEST
 expect;
*)

(* A constructor is used where it is declared: a compile-time
   constructor in compile-time code, and any other in run-time code. *)

exception $Stop
exception Failed
type t = ..
type t += $A | B
;;
[%%expect{|
exception $Stop
exception Failed
type t = ..
type t += $A | B
|}]

let x = Stop
;;
[%%expect{|
Line 1, characters 8-12:
1 | let x = Stop
            ^^^^
Error: The constructor "Stop" is declared in compile-time code,
       but this use is at run time.
|}]

let f g = try g () with Stop -> ()
;;
[%%expect{|
Line 1, characters 24-28:
1 | let f g = try g () with Stop -> ()
                            ^^^^
Error: The constructor "Stop" is declared in compile-time code,
       but this use is at run time.
|}]

let f = function A -> 1 | _ -> 0
;;
[%%expect{|
Line 1, characters 17-18:
1 | let f = function A -> 1 | _ -> 0
                     ^
Error: The constructor "A" is declared in compile-time code,
       but this use is at run time.
|}]

$let x = Failed
;;
[%%expect{|
Line 1, characters 9-15:
1 | $let x = Failed
             ^^^^^^
Error: The constructor "Failed" is declared at run time,
       but this use is in compile-time code.
       A constructor for compile-time code is declared with "$".
|}]

$let f g = try g () with Failed -> ()
;;
[%%expect{|
Line 1, characters 25-31:
1 | $let f g = try g () with Failed -> ()
                             ^^^^^^
Error: The constructor "Failed" is declared at run time,
       but this use is in compile-time code.
       A constructor for compile-time code is declared with "$".
|}]

$let f = function B -> 1 | _ -> 0
;;
[%%expect{|
Line 1, characters 18-19:
1 | $let f = function B -> 1 | _ -> 0
                      ^
Error: The constructor "B" is declared at run time,
       but this use is in compile-time code.
       A constructor for compile-time code is declared with "$".
|}]

let c = [%extension_constructor Stop]
;;
[%%expect{|
Line 1, characters 32-36:
1 | let c = [%extension_constructor Stop]
                                    ^^^^
Error: The constructor "Stop" is declared in compile-time code,
       but this use is at run time.
|}]

(* Quoted code is run-time code. *)

$let[..] g () = << raise Stop >>
;;
[%%expect{|
Line 1, characters 25-29:
1 | $let[..] g () = << raise Stop >>
                             ^^^^
Error: The constructor "Stop" is declared in compile-time code,
       but this use is at run time.
       The code a quotation builds runs in another program,
       which has no such constructor.
|}]

$let[..] g () = << raise Failed >>
;;
[%%expect{|
$val[..] g : unit -> 'a expr = <fun>
|}]

(* A rebinding mentions the constructor that it rebinds. *)

exception Broken = Stop
;;
[%%expect{|
Line 1, characters 19-23:
1 | exception Broken = Stop
                       ^^^^
Error: The constructor "Stop" is declared in compile-time code,
       but this use is at run time.
|}]

exception $Halt = Failed
;;
[%%expect{|
Line 1, characters 18-24:
1 | exception $Halt = Failed
                      ^^^^^^
Error: The constructor "Failed" is declared at run time,
       but this use is in compile-time code.
       A constructor for compile-time code is declared with "$".
|}]

type t += $B' = B
;;
[%%expect{|
Line 1, characters 16-17:
1 | type t += $B' = B
                    ^
Error: The constructor "B" is declared at run time,
       but this use is in compile-time code.
       A constructor for compile-time code is declared with "$".
|}]

(* A signature tells the two kinds of constructor apart. *)

module M : sig exception E end = struct exception $E end
;;
[%%expect{|
Line 1, characters 33-56:
1 | module M : sig exception E end = struct exception $E end
                                     ^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig exception $E end
       is not included in
         sig exception E end
       Extension declarations do not match:
         type exn += $E
       is not included in
         type exn += E
       One is a compile-time constructor, declared with "$",
       and the other an ordinary constructor.
|}]

module M : sig type t += $C end = struct type t += C end
;;
[%%expect{|
Line 1, characters 34-56:
1 | module M : sig type t += $C end = struct type t += C end
                                      ^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t += C end
       is not included in
         sig type t += $C end
       Extension declarations do not match:
         type t += C
       is not included in
         type t += $C
       One is a compile-time constructor, declared with "$",
       and the other an ordinary constructor.
|}]

(* A module in an expression cannot declare a compile-time
   constructor. *)

let f () = let module M = struct exception $E end in ()
;;
[%%expect{|
Line 1, characters 15-49:
1 | let f () = let module M = struct exception $E end in ()
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A locally bound module cannot have compile-time components
       (generators, static bindings, or compile-time constructors).
       Bind it at the top level of a structure instead.
|}]

(* A constructor declared in compile-time code is a constructor of
   that code. *)

$let x = let exception Local in try raise Local with Local -> 0
;;
[%%expect{|
>> Fatal error: structure items in staged code: not implemented
Uncaught exception: Misc.Fatal_error

|}]

let f () = let exception $E in ()
;;
[%%expect{|
Line 1, characters 15-27:
1 | let f () = let exception $E in ()
                   ^^^^^^^^^^^^
Error: A compile-time constructor cannot be declared in an expression.
       Declare it at the top level of a structure instead.
|}]
