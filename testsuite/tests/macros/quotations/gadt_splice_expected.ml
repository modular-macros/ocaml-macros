(* TEST
 expect;
*)

(* The expected type of a splice in the arm of a match that learns an
   equation.  The splice's body may not use the equation, which holds
   only once the match has happened; the expected type propagates into
   the body unless it mentions a type that the equation relates. *)

type (_, _) eql = Refl : ('a, 'a) eql
type t1 = { x : int }
type t2 = { x : int }
;;
[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
type t1 = { x : int; }
type t2 = { x : int; }
|}]

(* An equation unrelated to the expected type: the expected type
   propagates, and the label is that of t1. *)
let f (type a) (r : (a, int) eql expr) : t1 expr =
  << match $r with Refl -> $(<< { x = 1 } >>) >>
;;
[%%expect{|
val f : ('a, int) eql expr -> t1 expr = <fun>
|}]

(* The expected type mentions a side of the equation: the body is typed
   on its own, and its type, which needs the equation, is unified with
   the expected one in the arm. *)
let g (type a) (r : (a, int) eql expr) (c : a expr) : int expr =
  << match $r with Refl -> $c >>
;;
[%%expect{|
val g : ('a, int) eql expr -> 'a expr -> int expr = <fun>
|}]

let h (type a) (r : (a, int) eql expr) (c : a expr) : int expr =
  << match $r with Refl -> $(<< $c + 1 >>) >>
;;
[%%expect{|
val h : ('a, int) eql expr -> 'a expr -> int expr = <fun>
|}]

(* An equation whose right-hand side is the expected type: the body is
   typed on its own, and the label, resolved without the expected type,
   is the last one defined. *)
let k (type a) (r : (a, t1) eql expr) : t1 expr =
  << match $r with Refl -> $(<< { x = 1 } >>) >>
;;
[%%expect{|
Line 2, characters 27-45:
2 |   << match $r with Refl -> $(<< { x = 1 } >>) >>
                               ^^^^^^^^^^^^^^^^^^
Error: This expression has type "t2 expr"
       but an expression was expected of type "t1 expr"
       Type "t2" is not compatible with type "t1"
|}]

(* The same, with the expected type given in the splice *)
let k (type a) (r : (a, t1) eql expr) : t1 expr =
  << match $r with Refl -> $(<< ({ x = 1 } : t1) >>) >>
;;
[%%expect{|
val k : ('a, t1) eql expr -> t1 expr = <fun>
|}]
