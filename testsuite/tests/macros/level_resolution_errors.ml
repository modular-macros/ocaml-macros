(* TEST
 expect;
*)

(* Error behaviour of level-based name resolution: what does not
   resolve, and how it is reported. *)

(* A user value has no compile-world entry: a level -1 use is a level
   error (user globals are not cross-stage persistent). *)
let g = 1
macro bad () = Expr.int g
;;

[%%expect{|
val g : int = 1
Line 2, characters 24-25:
2 | macro bad () = Expr.int g
                            ^
Error: "g" is bound at run time, but this use is in compile-time code.
|}]

(* A name introduced by an open, used at a level no candidate serves:
   reported as a level mismatch, not as unbound. *)
module P = struct let pf x = x end
open P
let q = << pf >>
;;

[%%expect{|
module P : sig val pf : 'a -> 'a end
Line 3, characters 11-13:
3 | let q = << pf >>
               ^^
Error: "pf" is bound at run time, but this use is inside a quotation.
|}]

(* Stdlib values are registered at levels 0 and -1 only: a mention at
   a positive level (a quotation not realigned by a splice) is
   rejected the same way.  PRIMITIVES are the exception -- level-neutral
   like types and constructors, materialised inline at every use -- so
   [succ], an external, is accepted where an ordinary stdlib value is
   not. *)
let q2 = << succ >>
;;

[%%expect{|
val q2 : (int -> int) expr = <external>
|}]

let q3 = << List.map >>
;;

[%%expect{|
Line 1, characters 12-20:
1 | let q3 = << List.map >>
                ^^^^^^^^
Error: "List.map" is bound at run time, but this use is inside a quotation.
|}]

(* A lexical binding wins over a level-matched open candidate, even
   when the local is level-wrong: shadowing is hygienic, and the level
   judgment reports the local. *)
module Q = struct macro qf x = x + 1 end
open Q
let qf x = x + 2   (* shadows the macro, at level 0 *)
macro use () = Expr.int (qf 1)
;;

[%%expect{|
module Q : sig macro qf : int -> int end
val qf : int -> int = <fun>
Line 4, characters 25-27:
4 | macro use () = Expr.int (qf 1)
                             ^^
Error: "qf" is bound at run time, but this use is in compile-time code.
|}]

