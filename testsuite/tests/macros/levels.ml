(* TEST
 expect;
*)

macro tardy x = <<x>>
;;

[%%expect{|
Line 1, characters 18-19:
1 | macro tardy x = <<x>>
                      ^
Error: "x" is bound in compile-time code, but this use is at run time.
       A compile-time value reaches the code a quotation builds only
       through a splice ("$x") or a lift ("Expr.int").
|}]

macro hasty x = $x
;;

[%%expect{|
Line 1, characters 16-18:
1 | macro hasty x = $x
                    ^^
Error: A splice outside a quotation is evaluated at compile time,
       which is meaningful only at the top level of a module.
       This one is in compile-time code.
|}]


macro timely x = x
;;

[%%expect{|
macro timely : 'a -> 'a = <fun>
|}]

macro timely2 x = << $x >>
;;

[%%expect{|
macro timely2 : 'a expr -> 'a expr = <fun>
|}]

(* ------------------------------------------------------------------ *)
(* Definitions in staged code                                          *)
(* (LIMITATIONS.md, definitions-in-staged-code)                        *)
(* ------------------------------------------------------------------ *)

(* A value binding at the level of a structure is admitted only in
   ordinary run-time code.  Inside a quotation, the module that carries
   it may not define values ... *)

let e = << let module M = struct let x = 1 end in M.x >>
;;

[%%expect{|
Line 1, characters 33-42:
1 | let e = << let module M = struct let x = 1 end in M.x >>
                                     ^^^^^^^^^
Error: A module defined inside a quotation cannot define values or macros.
       Define the module outside the quotation and refer to it here.
|}]

(* ... nor macros, ... *)

let e = << let module M = struct macro m x = x end in M.m 1 >>
;;

[%%expect{|
Line 1, characters 33-46:
1 | let e = << let module M = struct macro m x = x end in M.m 1 >>
                                     ^^^^^^^^^^^^^
Error: A module defined inside a quotation cannot define values or macros.
       Define the module outside the quotation and refer to it here.
|}]

(* ... and the same holds of the other two forms that carry a structure
   into an expression. *)

let e = << let open struct let x = 1 end in x >>
;;

[%%expect{|
Line 1, characters 27-36:
1 | let e = << let open struct let x = 1 end in x >>
                               ^^^^^^^^^
Error: A module defined inside a quotation cannot define values or macros.
       Define the module outside the quotation and refer to it here.
|}]

module type S = sig val x : int end
let e = << (module struct let x = 1 end : S) >>
;;

[%%expect{|
module type S = sig val x : int end
Line 2, characters 26-35:
2 | let e = << (module struct let x = 1 end : S) >>
                              ^^^^^^^^^
Error: A module defined inside a quotation cannot define values or macros.
       Define the module outside the quotation and refer to it here.
|}]

(* A quotation inside a macro body is still a quotation, even though it
   sits at level 0. *)

macro m () = << let module M = struct let x = 1 end in M.x >>
;;

[%%expect{|
Line 1, characters 38-47:
1 | macro m () = << let module M = struct let x = 1 end in M.x >>
                                          ^^^^^^^^^
Error: A module defined inside a quotation cannot define values or macros.
       Define the module outside the quotation and refer to it here.
|}]

(* Compile-time code -- a macro body ... *)

macro m () = let module M = struct let x = 1 end in Expr.int M.x
;;

[%%expect{|
Line 1, characters 35-44:
1 | macro m () = let module M = struct let x = 1 end in Expr.int M.x
                                       ^^^^^^^^^
Error: A module defined in compile-time code (the body of a macro or
       of a splice) cannot define values or macros.
       Define it at the top level of a structure instead.
|}]

(* ... and a splice body -- gets the other half of the message. *)

let e = << $(let module M = struct let x = 1 end in Expr.int M.x) >>
;;

[%%expect{|
Line 1, characters 35-44:
1 | let e = << $(let module M = struct let x = 1 end in Expr.int M.x) >>
                                       ^^^^^^^^^
Error: A module defined in compile-time code (the body of a macro or
       of a splice) cannot define values or macros.
       Define it at the top level of a structure instead.
|}]

(* Only bindings are refused: a module of level-free components is
   fine in either place. *)

let e = << let module M = struct type t = int end in (1 : M.t) >>
;;

[%%expect{|
val e : int expr = <external>
|}]

macro m () = let module M = struct type t = int end in Expr.int (1 : M.t)
;;

[%%expect{|
macro m : unit -> int expr = <fun>
|}]
