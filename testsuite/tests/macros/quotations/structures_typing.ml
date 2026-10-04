(* TEST
 expect;
*)

(* What a structure in a quotation may define, and the levels of what
   it defines. *)

(* Values, in a quotation of run-time code too. *)
let a = << let module M = struct let y = 1  let z = y + 1 end in M.z >>
let b = << let open struct let y = 1 end in y >>
let c =
  << let module M = struct
       include struct let a = 1 end
       let b = a + 1
     end in
     M.b >>
;;

[%%expect{|
val a : int expr =
  <code
    (let (M = (let (y = 1 z = (+ y 1)) (makeblock 0 y z))) (field_imm 1 M))>
val b : int expr =
  <code
    (let (open = (let (y = 1) (makeblock 0 y)) y_1 = (field_mut 0 open)) y_1)>
val c : int expr =
  <code
    (let
      (M =
         (let
           (include = (let (a = 1) (makeblock 0 a))
            a_1 = (field_mut 0 include)
            b = (+ a_1 1))
           (makeblock 0 a_1 b)))
      (field_imm 1 M))>
|}]

(* The members are of the quotation's level, and so is what they
   mention. *)
let base = 1
let bad = << let open struct let y = base end in y >>
;;

[%%expect{|
val base : int = 1
Line 2, characters 37-41:
2 | let bad = << let open struct let y = base end in y >>
                                         ^^^^
Error: "base" is bound at run time, but this use is inside a quotation.
|}]

$let[..] bad () = let module M = struct let y = 1 end in << M.y >>
;;

[%%expect{|
Line 1, characters 60-63:
1 | $let[..] bad () = let module M = struct let y = 1 end in << M.y >>
                                                                ^^^
Error: The module "M" is bound in compile-time code, so the code this
       quotation builds cannot name it: that code runs in another
       program, which has no such module.
       Bind the module outside the generator or splice body, or move the
       reference into a splice.
|}]

(* Compile-time definitions stay refused: the code a quotation builds
   is run-time code. *)
$let[..] bad () = << let module M = struct $let x : int = 1 end in 1 >>
;;

[%%expect{|
Line 1, characters 43-59:
1 | $let[..] bad () = << let module M = struct $let x : int = 1 end in 1 >>
                                               ^^^^^^^^^^^^^^^^
Error: A module defined inside a quotation cannot define generators,
       static bindings or compile-time constructors:
       the code a quotation builds is run-time code.
       Define them at the top level of a structure instead.
|}]

$let[..] bad () = << let module M = struct $let[..] g () = << 1 >> end in 1 >>
;;

[%%expect{|
Line 1, characters 43-66:
1 | $let[..] bad () = << let module M = struct $let[..] g () = << 1 >> end in 1 >>
                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: A module defined inside a quotation cannot define generators,
       static bindings or compile-time constructors:
       the code a quotation builds is run-time code.
       Define them at the top level of a structure instead.
|}]

$let[..] bad () = << let module M = struct exception $E end in 1 >>
;;

[%%expect{|
Line 1, characters 43-55:
1 | $let[..] bad () = << let module M = struct exception $E end in 1 >>
                                               ^^^^^^^^^^^^
Error: A module defined inside a quotation cannot define generators,
       static bindings or compile-time constructors:
       the code a quotation builds is run-time code.
       Define them at the top level of a structure instead.
|}]

(* A package type has no compile-time members. *)
module type S = sig $val k : int  val v : int end
$let[..] bad () : (module S) expr = << assert false >>
;;

[%%expect{|
module type S = sig $val k : int val v : int end
Line 2, characters 26-27:
2 | $let[..] bad () : (module S) expr = << assert false >>
                              ^
Error: This module type cannot be used as a first-class module:
       it has compile-time components (generators,
       static bindings, or compile-time constructors).
|}]

(* A variable of the quotation may not escape through a member. *)
$let r : int expr ref = ref << 0 >>
$let[..] g () =
  << let module M = struct
       let y = 1
       let z = $(r := << y >>; << y + 1 >>)
     end in
     M.z >>
$let[..] h () = !r
let ok = $(g ())
;;

[%%expect{|
$val r : int expr ref = {contents = <code 0>}
$val[..] g : unit -> int expr = <fun>
$val[..] h : unit -> int expr = <fun>
val ok : int = 2
|}]

let bad = $(h ())
;;

[%%expect{|
Line 1, characters 12-16:
1 | let bad = $(h ())
                ^^^^
Error: The code built by this splice has a variable escaping a quotation's scope: y.
|}]
