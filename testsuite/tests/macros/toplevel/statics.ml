(* TEST
 toplevel;
*)

(* A static binding of a phrase is evaluated when the phrase is, and
   serves the compile-time code of later phrases and of its own. *)

$let x = print_endline "x is evaluated"; 10;;
let a = $(Expr.int (x + 1));;
$let y = x * 2  let b = $(Expr.int (x + y));;
$let rec fact n = if n = 0 then 1 else n * fact (n - 1);;
let c = $(Expr.int (fact 5));;

(* Each binding of a name is its own. *)

$let z = 1  let d = $(Expr.int z)  $let z = 2  let e = $(Expr.int z);;
$let z = "three";;
let f = $(Expr.string z);;

(* A static binding is not a run-time value. *)

let g = x;;
let h = << x >>;;

(* Static bindings in modules *)

module M = struct
  $let s = 5
  let v = $(Expr.int s)
  module N = struct $let t = s + 1 end
end;;
let i = $(Expr.int (M.s + M.N.t)) + M.v;;
module A = M;;
let j = $(Expr.int A.N.t);;
include M;;
let k = $(Expr.int s);;

(* A static binding holds its state across phrases. *)

$let counter = ref 0;;
let l = $(incr counter; Expr.int !counter);;
let m = $(incr counter; Expr.int !counter);;
