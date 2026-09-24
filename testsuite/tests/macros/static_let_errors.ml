(* TEST
 expect;
*)

(* The refusals around static bindings ($let), each located. *)

$let x = 41
;;

[%%expect{|
$val x : int = 41
|}]

(* A static serves at level -1 only: run-time use is refused. *)

let y = x + 1
;;

[%%expect{|
Line 1, characters 8-9:
1 | let y = x + 1
            ^
Error: "x" is bound in compile-time code, but this use is at run time.
|}]

(* ...and quotations are run-time code, so they cannot name it. *)

$gen m () : int expr = << x >>
;;

[%%expect{|
Line 1, characters 26-27:
1 | $gen m () : int expr = << x >>
                              ^
Error: "x" is bound in compile-time code, but this use is at run time.
       A compile-time value reaches the code a quotation builds only
       through a splice ("$x") or a lift ("Expr.int").
|}]

(* Compile-time use is the point. *)

$gen m () : int expr = Expr.int (x + 1)
;;
let a = $(m ())
;;

[%%expect{|
$gen m : unit -> int expr = <fun>
val a : int = 42
|}]

(* A $let is a structure item of level 0: not inside compile-time
   code. *)

$gen g () : int expr =
  let module M = struct $let z = 1 end in Expr.int M.z
;;

[%%expect{|
Line 2, characters 24-34:
2 |   let module M = struct $let z = 1 end in Expr.int M.z
                            ^^^^^^^^^^
Error: A module defined in compile-time code (the body of a generator
       or of a splice) cannot define generators or static bindings:
       they would sit one further level down.
       Define them at the top level of a structure instead.
|}]

(* D2: a quotation in a static initialiser may capture only global
   names; a local capture needs a $gen's environment. *)

let local = 7
;;
$let bad = << local + 1 >>
;;

[%%expect{|
val local : int = 7
Line 3, characters 0-26:
3 | $let bad = << local + 1 >>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The quotations in this static binding capture "local",
       a run-time name of the enclosing module or functor.
       A static value's quotations may only name run-time values
       of other compilation units (global values).
       To close over local run-time names, use a generator ($gen):
       its environment is delivered wherever the generator is called.
|}]

(* Nested in a module of a toplevel phrase: refused (the phrase's
   two-world region does not yet serve static members; batch
   compilation supports the same form, static_let.ml). *)

module M = struct $let z = 6 end
;;

[%%expect{|
Line 1, characters 18-28:
1 | module M = struct $let z = 6 end
                      ^^^^^^^^^^
Error: A static binding is not yet supported inside a module of a
       toplevel phrase.
       Bind it directly at the top level, or put the module in a
       compiled unit.
|}]

