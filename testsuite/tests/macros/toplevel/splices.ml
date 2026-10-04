(* TEST
 toplevel;
*)

(* A phrase with a top-level splice is evaluated in two steps: its
   compile-time code runs first and builds the phrase, which then runs
   as any phrase does. *)

let a = $(Expr.int (1 + 2));;
$(Expr.int 7);;
let _ = $(Expr.string "printed");;

(* The code may mention what earlier phrases bound, and what the phrase
   itself binds. *)

let b = a + 1;;
let c = $(<< a + b >>);;
let d = 10  let e = $(<< d + 1 >>);;
let f x = $(<< x + d >>);;
f 1;;

(* A later binding of the same name does not change what the code
   meant. *)

let d = "rebound";;
f 1;;

(* Splices in the bodies of functors, of local modules, of classes and
   of recursive modules *)

module F (X : sig val n : int end) = struct let m = $(Expr.int 5) + X.n end;;
module A = F (struct let n = 1 end);;
A.m;;
let g () = let module L = struct let w = $(Expr.int 6) end in L.w;;
g ();;
class k = object method m = $(Expr.int 9) end;;
(new k)#m;;
module rec R : sig val r : int end = struct let r = $(Expr.int 3) + S.s end
and S : sig val s : int end = struct let s = 1 end;;
R.r;;

(* What compile-time code prints is printed when the phrase is
   evaluated, before what the phrase prints. *)

let h = print_endline "at run time"; $(print_endline "at compile time"; << 1 >>);;

(* A quotation is a value of compile-time code. *)

<< 1 + 2 >>;;
